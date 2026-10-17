//! "Did you mean" suggestions for error messages.
//!
//! When a name in an error closely resembles one that was actually
//! available at the point of the error, the message suggests that name:
//! `Undefined variable 'resul'. Did you mean: 'result'?`
//!
//! The tables here hold the pure, state-independent part: the edit
//! distance matching plus the lookup tables for names and methods from
//! other languages. Gathering the candidate pools requires VM state and
//! lives in `vm::suggestions`; the compiler also uses `near_keyword`.

/// How many candidates are considered at most, bounding the error-path
/// scan.
pub(crate) const MAX_CANDIDATES: usize = 750;

/// Names longer than this are never suggested.
pub(crate) const MAX_STRING_SIZE: usize = 40;

/// The largest edit distance for which a candidate is still suggested.
const MAX_DISTANCE: usize = 2;

/// Byte-wise Levenshtein distance between `a` and `b`, bailing out with
/// `max + 1` as soon as the distance provably exceeds `max`.
///
/// Identifiers are ASCII, so bytes are characters for them. The few
/// non-ASCII names that can reach here (arbitrary strings via `getattr`,
/// or import paths) are compared byte-wise as a cheap approximation.
fn levenshtein_within(a: &[u8], b: &[u8], max: usize) -> usize {
    if a == b {
        return 0;
    }
    if a.len().abs_diff(b.len()) > max {
        return max + 1;
    }
    // Single-row dynamic programming: after processing a[..=i], row[j]
    // holds the distance between a[..=i] and b[..j].
    let mut row: Vec<usize> = (0..=b.len()).collect();
    for (i, a_byte) in a.iter().enumerate() {
        let mut diagonal = row[0];
        row[0] = i + 1;
        let mut row_min = i + 1;
        for (j, b_byte) in b.iter().enumerate() {
            let above = row[j + 1];
            let current = if a_byte == b_byte {
                diagonal
            } else {
                1 + diagonal.min(above).min(row[j])
            };
            row[j + 1] = current;
            diagonal = above;
            row_min = row_min.min(current);
        }
        if row_min > max {
            return max + 1;
        }
    }
    row[b.len()]
}

/// The candidate that most closely resembles `name`, if one is close
/// enough to plausibly be what was meant.
///
/// Very short names never get suggestions: at two characters and below,
/// almost anything is within editing distance, so a "did you mean" would
/// be noise rather than help (dataclass fields named `a` and `b` are
/// not typos of each other). For names below six characters only a
/// single edit is accepted for the same reason (`math` is not a typo of
/// `hash`). Dunder protocol names are machinery rather than user-facing
/// API, and their siblings (`__setitem__` vs `__getitem__`) are always
/// close enough that suggesting them would be noise.
pub(crate) fn did_you_mean(name: &str, candidates: &[&str]) -> Option<String> {
    if name.starts_with("__") || name.len() < 3 || name.len() > MAX_STRING_SIZE {
        return None;
    }
    let mut best: Option<(usize, &str)> = None;
    for candidate in candidates.iter().take(MAX_CANDIDATES) {
        if *candidate == name {
            // Through a class object, methods are not reachable, so the
            // missing name can exactly match a collected method
            // (`Foo.greet` with a `greet` method) - suggesting the very
            // name that just failed would be absurd.
            continue;
        }
        let distance = levenshtein_within(name.as_bytes(), candidate.as_bytes(), MAX_DISTANCE);
        let plausible = distance == 1 || (distance == 2 && name.len() >= 6);
        if plausible && best.is_none_or(|(best_distance, _)| distance < best_distance) {
            best = Some((distance, candidate));
        }
    }
    best.map(|(_, candidate)| candidate.to_string())
}

/// ` Did you mean: '...'?` suffix for an error message; empty when no
/// candidate is close enough.
pub(crate) fn did_you_mean_suffix(name: &str, candidates: &[&str]) -> String {
    match did_you_mean(name, candidates) {
        Some(candidate) => format!(" Did you mean: '{candidate}'?"),
        None => String::new(),
    }
}

/// Hint for calling a method that does not exist on a builtin type, but
/// is common in another language and has a direct generic equivalent.
/// When the equivalent is a language construct rather than a method,
/// the hint describes the construct.
pub(crate) fn method_alias_hint(class_name: &str, method: &str) -> Option<&'static str> {
    match (class_name, method) {
        // `len` is a builtin function in generic, not a method, so the
        // size queries of other languages all map to it.
        (_, "len" | "length" | "size") => Some("Use the 'len' function instead."),
        ("List", "push") => Some("Did you mean: 'append'?"),
        ("Dict", "put") => Some("Use d[k] = v."),
        ("Dict", "has_key") => Some("Did you mean: 'contains'?"),
        ("Dict", "delete") => Some("Did you mean: 'pop'?"),
        ("Set", "add") => Some("Did you mean: 'insert'?"),
        ("Tuple", "append" | "push") => Some("Did you mean to use a 'list' instead?"),
        ("String", "format") => Some("Use an f-string instead."),
        ("String", "index") => Some("Did you mean: 'find'?"),
        _ => None,
    }
}

/// Hint for reading a builtin name that does not exist in generic but
/// has a direct equivalent in another language (chiefly Python), used
/// when no actual name is close enough to suggest.
pub(crate) fn other_language_name_hint(name: &str) -> Option<&'static str> {
    match name {
        "self" => Some("Did you mean 'this'?"),
        "None" => Some("Did you mean 'nil'?"),
        "True" => Some("Did you mean 'true'?"),
        "False" => Some("Did you mean 'false'?"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levenshtein_distances() {
        assert_eq!(levenshtein_within(b"prin", b"print", 2), 1);
        assert_eq!(levenshtein_within(b"result", b"resul", 2), 1);
        // A case typo is an ordinary substitution.
        assert_eq!(levenshtein_within(b"Print", b"print", 2), 1);
        assert_eq!(levenshtein_within(b"classes", b"class", 2), 2);
        assert_eq!(levenshtein_within(b"abc", b"xyz", 2), 3);
        assert_eq!(levenshtein_within(b"abc", b"abc", 2), 0);
        assert_eq!(levenshtein_within(b"horse", b"ros", 5), 3);
        assert_eq!(levenshtein_within(b"ros", b"horse", 5), 3);
    }

    #[test]
    fn did_you_mean_picks_the_closest_candidate() {
        let candidates = ["result", "results", "reset"];
        assert_eq!(
            did_you_mean("resul", &candidates).as_deref(),
            Some("result")
        );
        assert_eq!(
            did_you_mean("resullts", &candidates).as_deref(),
            Some("results")
        );
    }

    #[test]
    fn did_you_mean_prefers_the_first_candidate_at_equal_distance() {
        // Both are one edit away; the first candidate wins.
        assert_eq!(did_you_mean("abc", &["abd", "abe"]).as_deref(), Some("abd"));
    }

    #[test]
    fn did_you_mean_prefers_a_closer_candidate_over_an_earlier_one() {
        // `reset` is plausible at distance two, but `result` comes later
        // and matches at distance one, so it wins.
        let candidates = ["reset", "result"];
        assert_eq!(
            did_you_mean("resutt", &candidates).as_deref(),
            Some("result")
        );
    }

    #[test]
    fn did_you_mean_ignores_exact_and_distant_names() {
        // An exact match cannot be the explanation for the error.
        assert_eq!(did_you_mean("result", &["result"]), None);
        assert_eq!(did_you_mean("something", &["unrelated", "entirely"]), None);
        // Very short names never suggest.
        assert_eq!(did_you_mean("ab", &["ac"]), None);
        // A two-edit distance is only plausible for longer names:
        // `math` is not a typo of `hash`.
        assert_eq!(did_you_mean("math", &["hash"]), None);
        assert_eq!(did_you_mean("foo", &["bool"]), None);
        // ... while it stays acceptable for longer ones.
        assert_eq!(
            did_you_mean("classes", &["class"]).as_deref(),
            Some("class")
        );
    }

    #[test]
    fn did_you_mean_skips_dunder_names() {
        // Sibling dunders are always close; suggesting them is noise.
        assert_eq!(did_you_mean("__setitem__", &["__getitem__"]), None);
    }

    #[test]
    fn did_you_mean_handles_case_typos() {
        assert_eq!(
            did_you_mean("Print", &["print", "input"]).as_deref(),
            Some("print")
        );
    }

    #[test]
    fn did_you_mean_skips_the_exact_name_but_keeps_looking() {
        // A candidate that equals the name must not be suggested, but
        // the remaining candidates are still considered.
        assert_eq!(did_you_mean("greet", &["greet"]), None);
        assert_eq!(
            did_you_mean("greet", &["greet", "greets"]).as_deref(),
            Some("greets")
        );
    }

    #[test]
    fn did_you_mean_compares_non_ascii_names_byte_wise() {
        // An accented name matches its ASCII spelling: the extra byte of
        // the accent costs the two edits a long name may have.
        assert_eq!(did_you_mean("wörld", &["world"]).as_deref(), Some("world"));
        // Genuinely different scripts never get spurious suggestions.
        assert_eq!(did_you_mean("日本語", &["world", "result"]), None);
    }

    #[test]
    fn method_aliases_cover_common_other_language_names() {
        assert_eq!(
            method_alias_hint("List", "push"),
            Some("Did you mean: 'append'?")
        );
        assert_eq!(method_alias_hint("Dict", "put"), Some("Use d[k] = v."));
        assert_eq!(method_alias_hint("String", "upper"), None);
        assert_eq!(method_alias_hint("List", "append"), None);
    }

    #[test]
    fn other_language_names_have_hints() {
        assert_eq!(
            other_language_name_hint("self"),
            Some("Did you mean 'this'?")
        );
        assert_eq!(
            other_language_name_hint("None"),
            Some("Did you mean 'nil'?")
        );
        assert_eq!(other_language_name_hint("result"), None);
    }
}

//! Collection of small utility types.

// Should probably move all of the other Shrinkwrapped types in here as well.

use shrinkwraprs::Shrinkwrap;
use std::cmp::Ordering;
use std::fmt::Write;
use strum_macros::{Display, EnumIter};

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug, PartialOrd)]
#[shrinkwrap(mutable)]
pub(crate) struct Line(pub usize);

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug, PartialOrd)]
#[shrinkwrap(mutable)]
pub(crate) struct Column(pub usize);

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd)]
pub(crate) struct Location {
    pub(super) start_line: Line,
    pub(super) start_column: Column,
    pub(super) end_line: Line,
    pub(super) end_column: Column,
    pub(super) index: usize, // Starting byte index in source
}

impl Default for Location {
    fn default() -> Self {
        Self {
            start_line: Line(1),
            end_line: Line(1),
            start_column: Column(1),
            end_column: Column(1),
            index: 0,
        }
    }
}

impl Location {
    pub(crate) fn merge_ordered(&self, other: &Self) -> Self {
        debug_assert!(
            self.start_line < other.start_line
                || (self.start_line == other.start_line && self.start_column <= other.start_column),
            "merge_ordered called with self not before other (start)"
        );
        debug_assert!(
            self.end_line < other.end_line
                || (self.end_line == other.end_line && self.end_column <= other.end_column),
            "merge_ordered called with self not before other (end)"
        );

        Self {
            start_line: self.start_line,
            start_column: self.start_column,
            end_line: other.end_line,
            end_column: other.end_column,
            index: self.index,
        }
    }
}

/// If `loc` covers `line_no`, mark the corresponding chars in `underline`
/// with `ch`.
///
/// Shared by the runtime traceback rendering and the compile error
/// snippets.
pub(crate) fn mark_location_on_line(
    underline: &mut [char],
    loc: &Location,
    line_no: usize,
    line_len: usize,
    ch: char,
) {
    let start_line = *loc.start_line;
    let end_line = *loc.end_line;

    if line_no < start_line || line_no > end_line {
        return;
    }

    let start = if line_no == start_line {
        (*loc.start_column).saturating_sub(1)
    } else {
        0
    };
    let end = if line_no == end_line {
        (*loc.end_column).saturating_sub(1).min(line_len) // subtract 1 for exclusive
    } else {
        line_len
    };

    for item in underline.iter_mut().take(end).skip(start) {
        *item = ch;
    }
}

/// Render the source lines covered by `oploc` as snippet rows into
/// `out`: one 4-space indented source line followed by its 4-space
/// indented underline row, `~` marking the preceding and following
/// context and `^` the blamed span.
///
/// Row pairs are separated by a newline; the caller places the first
/// row (the runtime writes it right after the frame header, compile
/// errors after a newline of their own). Empty source lines are
/// skipped, underline rows are trimmed at the end. Shared by the
/// runtime traceback rendering and the compile error snippets.
pub(crate) fn render_snippet_rows(out: &mut String, source: &str, oploc: &OpcodeLocation) {
    let first_line = oploc
        .preceding
        .as_ref()
        .map_or(*oploc.source.start_line, |preceding| *preceding.start_line);

    let last_line = oploc
        .following
        .as_ref()
        .map_or(*oploc.source.end_line, |following| *following.end_line);

    let lines: Vec<&str> = source.lines().collect();
    let mut first_row = true;
    for line_no in first_line..=last_line {
        let line = lines.get(line_no.saturating_sub(1)).copied().unwrap_or("");
        let line_len = line.chars().count();
        if line_len == 0 {
            continue;
        }
        if !first_row {
            out.push('\n');
        }
        first_row = false;
        let _ = writeln!(out, "    {line}");
        let mut underline: Vec<char> = vec![' '; line_len];
        if let Some(preceding) = &oploc.preceding {
            mark_location_on_line(&mut underline, preceding, line_no, line_len, '~');
        }
        if let Some(following) = &oploc.following {
            mark_location_on_line(&mut underline, following, line_no, line_len, '~');
        }
        mark_location_on_line(&mut underline, &oploc.source, line_no, line_len, '^');
        let underline: String = underline
            .into_iter()
            .collect::<String>()
            .trim_end()
            .to_string();
        let _ = write!(out, "    {underline}");
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Default)]
pub(crate) struct OpcodeLocation {
    pub(super) preceding: Option<Location>,
    pub(super) source: Location,
    pub(super) following: Option<Location>,
}

impl OpcodeLocation {
    pub(crate) fn new(source: Location) -> Self {
        Self {
            preceding: None,
            source,
            following: None,
        }
    }

    /// Returns a version where preceding/following are extended to touch the source
    pub(crate) fn filled(&self) -> Self {
        let preceding = self.preceding.as_ref().map(|pre| Location {
            start_line: pre.start_line,
            start_column: pre.start_column,
            end_line: self.source.start_line,
            end_column: self.source.start_column,
            index: pre.index,
        });

        let following = self.following.as_ref().map(|fol| Location {
            start_line: self.source.end_line,
            start_column: self.source.end_column,
            end_line: fol.end_line,
            end_column: fol.end_column,
            index: fol.index,
        });

        Self {
            preceding,
            source: self.source,
            following,
        }
    }
}

/// Enum for variable mutability
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Mutability {
    Mutable,
    Immutable,
}

/// Enum for conditional statement types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ConditionType {
    If,
    Unless,
}

/// Enum for loop statement types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoopType {
    While,
    Until,
}

/// Enum for conditional jump directions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum JumpCondition {
    IfTrue,
    IfFalse,
}

impl From<JumpCondition> for bool {
    fn from(condition: JumpCondition) -> Self {
        match condition {
            JumpCondition::IfTrue => true,
            JumpCondition::IfFalse => false,
        }
    }
}

/// Enum for number encoding types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NumberEncoding {
    Short,
    Long,
}

/// Which shape of injected source `eval`/`exec` compiles and runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, EnumIter)]
pub(crate) enum InjectedKind {
    #[strum(serialize = "eval")]
    Eval,
    #[strum(serialize = "exec")]
    Exec,
}

impl InjectedKind {
    /// The name the compiled function carries; also how injected frames
    /// are recognized in stack traces.
    pub(crate) fn function_name(self) -> String {
        format!("<{self}>")
    }
}

/// Enum for function return modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReturnMode {
    Normal,
    Raw,
}

/// Enum for collection types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CollectionType {
    Dict,
    Set,
}

/// Enum for equality comparison modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EqualityMode {
    Equal,
    NotEqual,
}

/// Enum for the ordering comparison operators (`<`, `<=`, `>`, `>=`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Comparison {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl Comparison {
    pub(crate) const fn method_name(self) -> &'static str {
        match self {
            Self::Less => "__lt__",
            Self::LessEqual => "__le__",
            Self::Greater => "__gt__",
            Self::GreaterEqual => "__ge__",
        }
    }

    /// Whether `ordering` (of the left operand relative to the right) satisfies
    /// this operator.
    pub(crate) const fn holds_for(self, ordering: Ordering) -> bool {
        match self {
            Self::Less => matches!(ordering, Ordering::Less),
            Self::LessEqual => matches!(ordering, Ordering::Less | Ordering::Equal),
            Self::Greater => matches!(ordering, Ordering::Greater),
            Self::GreaterEqual => matches!(ordering, Ordering::Greater | Ordering::Equal),
        }
    }
}

/// Enum for range boundary types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RangeType {
    Inclusive,
    Exclusive,
}

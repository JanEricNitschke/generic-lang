//! `list.sort()`: an adaptive, stable, natural mergesort (powersort).
//!
//! Natural runs are detected (strictly descending ones reversed in place),
//! short runs are boosted to a minimum length by binary insertion, and adjacent
//! runs are merged with a galloping merge in the order dictated by the
//! powersort policy: each run boundary gets an integer "power" (its depth in a
//! conceptual balanced merge tree over the run midpoints), and boundaries with
//! higher power merge first.
//!
//! The sort never moves the values themselves. The elements (and, with a key
//! function, their computed keys) are rooted on the VM stack so the garbage
//! collector sees them across re-entrant `__lt__`/key calls, and the algorithm
//! permutes a `Vec<usize>` of indices into that region. The permutation is kept
//! a valid rearrangement of `0..len` at every point where an error can escape,
//! so the list can always be materialized without losing or duplicating
//! elements - fully sorted on success, in some unspecified order on failure.

use crate::value::{Instance, List, Value};
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::{
    VM,
    errors::{VmErrorKind, VmResult},
};

/// Minimum run length for powersort: natural runs shorter than this are
/// extended with binary insertion. Equals `list_len` when it is below 64,
/// otherwise a value in `[32, 64]` chosen so that `list_len` divided by it is
/// close to, but strictly below, a power of two.
fn merge_compute_minrun(mut list_len: usize) -> usize {
    let mut carry = 0;
    while list_len >= 64 {
        carry |= list_len & 1;
        list_len /= 2;
    }
    list_len + carry
}

/// Power of the boundary between adjacent runs `run1 = [base, base + len_a)`
/// and `run2 = [base + len_a, base + len_a + len_b)` within `total_list_len`
/// elements. A higher power marks a boundary that must be merged earlier.
/// Requires `len_a >= 1`, `len_b >= 1`, and
/// `base + len_a + len_b <= total_list_len`.
fn node_power(base: usize, len_a: usize, len_b: usize, total_list_len: usize) -> u32 {
    debug_assert!(len_a >= 1 && len_b >= 1, "runs are never empty");
    debug_assert!(
        base + len_a + len_b <= total_list_len,
        "adjacent runs lie within the list"
    );
    let mut a = 2 * base + len_a;
    let mut b = a + len_a + len_b;
    let mut power = 0;
    loop {
        power += 1;
        if a >= total_list_len {
            a -= total_list_len;
            b -= total_list_len;
        } else if b >= total_list_len {
            break;
        }
        a *= 2;
        b *= 2;
    }
    power
}

/// A pending run on the powersort merge stack, addressing `perm[base..base+len]`.
/// `power` is the boundary power against the run that follows it on the stack.
#[derive(Clone, Copy)]
struct Run {
    base: usize,
    len: usize,
    power: u32,
}

/// Consecutive wins by one run that trip a merge into galloping mode, and the
/// initial value of the adaptive `min_gallop` threshold.
const MIN_GALLOP: usize = 7;

/// Which backing array a run being merged currently occupies: `Perm` is the
/// permutation itself, `Scratch` is the temporary copy that `merge_lo` and
/// `merge_hi` make of the smaller run.
#[derive(Clone, Copy)]
enum Side {
    Perm,
    Scratch,
}

/// Evaluate a fallible comparison inside a merge loop; on error, break out of
/// the labelled loop carrying it so the flush after the loop still runs and
/// leaves `perm` a whole permutation.
macro_rules! guard {
    ($label:lifetime, $result:expr) => {
        match $result {
            Ok(value) => value,
            Err(error) => break $label Err(error),
        }
    };
}

/// Stable powersort over values living on the VM stack at
/// `stack[keys_base .. keys_base + perm.len()]`, which the caller pushes there
/// so the garbage collector keeps them reachable across re-entrant comparisons.
/// Only the index permutation, the merge scratch, and the run stack live here;
/// they hold plain indices, so a comparison that re-enters the interpreter can
/// never leave an unrooted value behind.
struct Sorter<'vm> {
    /// Interpreter handle, used to compare two keys (which may run a
    /// user-defined `__lt__` and re-enter the VM).
    vm: &'vm mut VM,
    /// Offset into `vm.stack` of the first key. Key `i` is
    /// `vm.stack[keys_base + i]`. When no `key` function was given this
    /// coincides with the elements' own region, so keys are the elements
    /// themselves.
    keys_base: usize,
    /// The sort order, as a permutation of `0..len`. This is what the algorithm
    /// actually rearranges: every run detection, reversal, and merge moves these
    /// indices, never the values, so a re-entrant comparison never holds an
    /// unrooted `Value`. `perm[rank]` is the key/element index that ends up at
    /// position `rank`; the caller materialises the result by reading its
    /// rooted element region at offset `perm[rank]` for each `rank`.
    /// Comparisons dereference one level of indirection: to order positions
    /// `a` and `b`, compare `keys[perm[a]]` against `keys[perm[b]]`.
    perm: Vec<usize>,
    /// Reusable staging buffer for merges. Merging two adjacent runs of `perm`
    /// in place is not possible, so the smaller run is copied here first and
    /// merged back into `perm`. Holds indices (like `perm`), so it needs no
    /// rooting. Cleared and refilled per merge; a field so that its capacity is
    /// reused across the many merges of one sort.
    scratch: Vec<usize>,
    /// The powersort run stack: runs discovered so far but not yet merged, each
    /// recording its span in `perm` and its boundary power. `found_new_run`
    /// merges from the top of this stack according to the powersort policy, and
    /// `merge_force_collapse` drains it at the end.
    pending: Vec<Run>,
    /// Adaptive galloping threshold: how many consecutive wins by one run trip a
    /// merge into galloping mode. Raised when galloping fails to pay off and
    /// lowered while it does, carried across every merge of one sort.
    min_gallop: usize,
}

impl Sorter<'_> {
    /// `keys[left] < keys[right]`, respecting a user-defined `__lt__`. The
    /// operands are read straight from the rooted stack region.
    fn lt(&mut self, left: usize, right: usize) -> VmResult<bool> {
        let left_key = self.vm.stack[self.keys_base + left];
        let right_key = self.vm.stack[self.keys_base + right];
        self.vm.compare_values_lt(left_key, right_key)
    }

    /// `keys[left] <= keys[right]`, derived from `lt` so ordering stays defined
    /// by the single `__lt__` primitive.
    fn le(&mut self, left: usize, right: usize) -> VmResult<bool> {
        Ok(!self.lt(right, left)?)
    }

    /// The `perm` entry (an index into the key region) at `index` of `side`.
    fn entry(&self, side: Side, index: usize) -> usize {
        match side {
            Side::Perm => self.perm[index],
            Side::Scratch => self.scratch[index],
        }
    }

    /// Binary insertion sort of `perm[base..base+len]`, given that the first
    /// `presorted` elements (always at least one) are already ordered.
    fn binary_insertion_sort(&mut self, base: usize, len: usize, presorted: usize) -> VmResult<()> {
        debug_assert!(
            presorted >= 1,
            "the natural run is always at least one element"
        );
        for unsorted in (base + presorted)..(base + len) {
            let pivot = self.perm[unsorted];
            let mut low = base;
            let mut high = unsorted;
            while low < high {
                let mid = usize::midpoint(low, high);
                let probe = self.perm[mid];
                if self.lt(pivot, probe)? {
                    high = mid;
                } else {
                    low = mid + 1;
                }
            }
            // `pivot` sits at `unsorted`; rotating brings it to `low` and slides
            // the intervening elements up by one.
            self.perm[low..=unsorted].rotate_right(1);
        }
        Ok(())
    }

    /// Length of the natural run at `perm[base..]`, scanning at most `remaining`
    /// elements. A run is either the longest ascending stretch, with
    ///
    /// ```text
    /// keys[0] <= keys[1] <= keys[2] <= ...
    /// ```
    ///
    /// or the longest descending stretch, with
    ///
    /// ```text
    /// keys[0] > keys[1] > keys[2] > ...
    /// ```
    ///
    /// Returns `(run_len, descending)`, where `descending` is `false` in the
    /// ascending case and `true` in the descending one. The descending case is
    /// deliberately strict: the caller reverses a descending run in place, and
    /// strict `>` guarantees there are no equal elements whose relative order
    /// the reversal would flip, so stability is preserved. Requires
    /// `remaining >= 1`.
    fn count_run(&mut self, base: usize, remaining: usize) -> VmResult<(usize, bool)> {
        debug_assert!(remaining >= 1, "a run is only sought below the list end");
        if remaining == 1 {
            return Ok((1, false));
        }
        let (second, first) = (self.perm[base + 1], self.perm[base]);
        let descending = self.lt(second, first)?;
        let mut run_len = 2;
        while run_len < remaining {
            let (current, previous) = (self.perm[base + run_len], self.perm[base + run_len - 1]);
            if self.lt(current, previous)? == descending {
                run_len += 1;
            } else {
                break;
            }
        }
        Ok((run_len, descending))
    }

    /// Find where `key_entry` belongs within the run `side[base .. base+len]`,
    /// searching outward from `hint`. Returns the offset `k` in `0..=len` such
    /// that the first `k` entries all order before `key_entry` (before or equal
    /// when `rightmost`). An exponential search from the hint brackets the
    /// answer, then a binary search pins it, so a run that keeps winning is
    /// consumed in `O(log k)` comparisons.
    fn gallop(
        &mut self,
        key_entry: usize,
        side: Side,
        base: usize,
        len: usize,
        hint: usize,
        rightmost: bool,
    ) -> VmResult<usize> {
        debug_assert!(hint < len);
        let mut last_ofs = 0;
        let mut ofs = 1;
        let hinted = self.entry(side, base + hint);
        let (mut lo, mut hi) = if self.gallop_lower(hinted, key_entry, rightmost)? {
            // `key_entry` lies to the right of the hint; widen rightward.
            let max_ofs = len - hint;
            while ofs < max_ofs {
                let probe = self.entry(side, base + hint + ofs);
                if self.gallop_lower(probe, key_entry, rightmost)? {
                    last_ofs = ofs;
                    ofs = ofs.checked_mul(2).map_or(max_ofs, |doubled| doubled + 1);
                } else {
                    break;
                }
            }
            ofs = ofs.min(max_ofs);
            (hint + last_ofs + 1, hint + ofs)
        } else {
            // `key_entry` lies at or to the left of the hint; widen leftward.
            let max_ofs = hint + 1;
            while ofs < max_ofs {
                let probe = self.entry(side, base + hint - ofs);
                if self.gallop_lower(probe, key_entry, rightmost)? {
                    break;
                }
                last_ofs = ofs;
                ofs = ofs.checked_mul(2).map_or(max_ofs, |doubled| doubled + 1);
            }
            ofs = ofs.min(max_ofs);
            (hint + 1 - ofs, hint - last_ofs)
        };
        while lo < hi {
            let mid = usize::midpoint(lo, hi);
            let probe = self.entry(side, base + mid);
            if self.gallop_lower(probe, key_entry, rightmost)? {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        Ok(hi)
    }

    /// Gallop search predicate: `keys[probe] < key` normally, or
    /// `keys[probe] <= key` when locating the rightmost valid position.
    fn gallop_lower(&mut self, probe: usize, key_entry: usize, rightmost: bool) -> VmResult<bool> {
        if rightmost {
            self.le(probe, key_entry)
        } else {
            self.lt(probe, key_entry)
        }
    }

    /// Merge adjacent runs when the left run (`perm[a_base .. a_base+a_len]`) is
    /// no longer than the right (`perm[a_base+a_len .. +b_len]`). The left run is
    /// copied into `scratch`, then merged back into `perm` from the low end,
    /// switching to galloping once one run wins `min_gallop` times in a row. The
    /// caller has trimmed the runs so the right run's first element precedes the
    /// left run's.
    // The final `take_*` of each flush advances cursors that are never read
    // again; those dead stores are expected.
    #[allow(
        unused_assignments,
        clippy::too_many_lines,
        clippy::cognitive_complexity
    )]
    fn merge_lo(&mut self, a_base: usize, a_len: usize, b_len: usize) -> VmResult<()> {
        debug_assert!(a_len > 0 && b_len > 0, "runs are never empty");
        debug_assert!(a_len <= b_len, "the copied left run is the smaller one");
        let mut min_gallop = self.min_gallop;
        self.scratch.clear();
        self.scratch
            .extend_from_slice(&self.perm[a_base..a_base + a_len]);
        let mut a_at = 0; // cursor into scratch (left run)
        let mut a_rem = a_len;
        let mut b_at = a_base + a_len; // cursor into perm (right run)
        let mut b_rem = b_len;
        let mut dest = a_base;

        // Move the next `n` elements of a run to `dest`, advancing its cursor:
        // the left run is copied from `scratch`, the right run moved within
        // `perm`. `n == 1` is the one-at-a-time step, `n > 1` a gallop block.
        macro_rules! take_a {
            ($n:expr) => {{
                let n = $n;
                self.perm[dest..dest + n].copy_from_slice(&self.scratch[a_at..a_at + n]);
                dest += n;
                a_at += n;
                a_rem -= n;
            }};
        }
        macro_rules! take_b {
            ($n:expr) => {{
                let n = $n;
                self.perm.copy_within(b_at..b_at + n, dest);
                dest += n;
                b_at += n;
                b_rem -= n;
            }};
        }

        // The right run's first element is the overall smallest, so it lands first.
        take_b!(1);

        let outcome: VmResult<()> = if a_rem != 1 && b_rem != 0 {
            'merge: loop {
                let mut acount = 0;
                let mut bcount = 0;
                // One-at-a-time until a run wins consistently.
                loop {
                    let b_entry = self.perm[b_at];
                    let a_entry = self.scratch[a_at];
                    if guard!('merge, self.lt(b_entry, a_entry)) {
                        take_b!(1);
                        if b_rem == 0 {
                            break 'merge Ok(());
                        }
                        bcount += 1;
                        acount = 0;
                        if bcount >= min_gallop {
                            break;
                        }
                    } else {
                        take_a!(1);
                        if a_rem == 1 {
                            break 'merge Ok(());
                        }
                        acount += 1;
                        bcount = 0;
                        if acount >= min_gallop {
                            break;
                        }
                    }
                }
                // Galloping until neither run wins consistently anymore.
                min_gallop += 1;
                loop {
                    if min_gallop > 1 {
                        min_gallop -= 1;
                    }
                    self.min_gallop = min_gallop;

                    let b_entry = self.perm[b_at];
                    acount =
                        guard!('merge, self.gallop(b_entry, Side::Scratch, a_at, a_rem, 0, true));
                    take_a!(acount);
                    if a_rem <= 1 {
                        break 'merge Ok(());
                    }

                    take_b!(1);
                    if b_rem == 0 {
                        break 'merge Ok(());
                    }

                    let a_entry = self.scratch[a_at];
                    bcount =
                        guard!('merge, self.gallop(a_entry, Side::Perm, b_at, b_rem, 0, false));
                    take_b!(bcount);
                    if b_rem == 0 {
                        break 'merge Ok(());
                    }

                    take_a!(1);
                    if a_rem == 1 {
                        break 'merge Ok(());
                    }

                    if acount < MIN_GALLOP && bcount < MIN_GALLOP {
                        break;
                    }
                }
                min_gallop += 1;
                self.min_gallop = min_gallop;
            }
        } else {
            Ok(())
        };

        // Flush the remainder even on error: the right run is already in place,
        // the left run comes from scratch. This restores `perm` to a full
        // permutation before returning the (possibly failing) outcome.
        take_b!(b_rem);
        take_a!(a_rem);
        outcome
    }

    /// Merge adjacent runs when the left run (`perm[a_base .. a_base+a_len]`) is
    /// longer than the right (`perm[a_base+a_len .. +b_len]`). The right run is
    /// copied into `scratch`, then merged back into `perm` from the high end,
    /// switching to galloping once one run wins `min_gallop` times in a row. The
    /// caller has trimmed the runs so the left run's last element follows the
    /// right run's.
    // The final `take_*` of each flush advances cursors that are never read
    // again; those dead stores are expected.
    #[allow(
        unused_assignments,
        clippy::too_many_lines,
        clippy::cognitive_complexity
    )]
    fn merge_hi(&mut self, a_base: usize, a_len: usize, b_len: usize) -> VmResult<()> {
        debug_assert!(a_len > 0 && b_len > 0, "runs are never empty");
        debug_assert!(a_len > b_len, "the copied right run is the smaller one");
        let mut min_gallop = self.min_gallop;
        let b_base = a_base + a_len;
        self.scratch.clear();
        self.scratch
            .extend_from_slice(&self.perm[b_base..b_base + b_len]);
        let mut a_rem = a_len; // left run in perm at [a_base .. a_base+a_rem]
        let mut b_rem = b_len; // right run in scratch at [0 .. b_rem]
        let mut dest = b_base + b_len; // writes move downward from here

        // Move the next `n` elements of a run to just below `dest`, advancing
        // downward: the left run is moved within `perm`, the right run copied
        // from `scratch`. Both runs are consumed from their high end. `n == 1`
        // is the one-at-a-time step, `n > 1` a gallop block.
        macro_rules! take_a {
            ($n:expr) => {{
                let n = $n;
                dest -= n;
                self.perm
                    .copy_within(a_base + a_rem - n..a_base + a_rem, dest);
                a_rem -= n;
            }};
        }
        macro_rules! take_b {
            ($n:expr) => {{
                let n = $n;
                dest -= n;
                self.perm[dest..dest + n].copy_from_slice(&self.scratch[b_rem - n..b_rem]);
                b_rem -= n;
            }};
        }

        // The left run's last element is the overall largest, so it lands last.
        take_a!(1);

        let outcome: VmResult<()> = if a_rem != 0 && b_rem != 1 {
            'merge: loop {
                let mut acount = 0;
                let mut bcount = 0;
                // One-at-a-time until a run wins consistently.
                loop {
                    let a_entry = self.perm[a_base + a_rem - 1];
                    let b_entry = self.scratch[b_rem - 1];
                    if guard!('merge, self.lt(b_entry, a_entry)) {
                        take_a!(1);
                        if a_rem == 0 {
                            break 'merge Ok(());
                        }
                        acount += 1;
                        bcount = 0;
                        if acount >= min_gallop {
                            break;
                        }
                    } else {
                        take_b!(1);
                        if b_rem == 1 {
                            break 'merge Ok(());
                        }
                        bcount += 1;
                        acount = 0;
                        if bcount >= min_gallop {
                            break;
                        }
                    }
                }
                // Galloping until neither run wins consistently anymore.
                min_gallop += 1;
                loop {
                    if min_gallop > 1 {
                        min_gallop -= 1;
                    }
                    self.min_gallop = min_gallop;

                    let b_entry = self.scratch[b_rem - 1];
                    let k = guard!(
                        'merge,
                        self.gallop(b_entry, Side::Perm, a_base, a_rem, a_rem - 1, true)
                    );
                    acount = a_rem - k;
                    take_a!(acount);
                    if a_rem == 0 {
                        break 'merge Ok(());
                    }

                    take_b!(1);
                    if b_rem == 1 {
                        break 'merge Ok(());
                    }

                    let a_entry = self.perm[a_base + a_rem - 1];
                    let k = guard!(
                        'merge,
                        self.gallop(a_entry, Side::Scratch, 0, b_rem, b_rem - 1, false)
                    );
                    bcount = b_rem - k;
                    take_b!(bcount);
                    if b_rem <= 1 {
                        break 'merge Ok(());
                    }

                    take_a!(1);
                    if a_rem == 0 {
                        break 'merge Ok(());
                    }

                    if acount < MIN_GALLOP && bcount < MIN_GALLOP {
                        break;
                    }
                }
                min_gallop += 1;
                self.min_gallop = min_gallop;
            }
        } else {
            Ok(())
        };

        // Flush the remainder even on error: the left run is already in place,
        // the right run comes from scratch. This restores `perm` to a full
        // permutation before returning the (possibly failing) outcome.
        take_a!(a_rem);
        take_b!(b_rem);
        outcome
    }

    /// Merge the two adjacent runs at stack positions `lower` and `lower + 1`.
    fn merge_at(&mut self, lower: usize) -> VmResult<()> {
        let left_run = self.pending[lower];
        let right_run = self.pending[lower + 1];
        // The combined run always becomes the new top of the stack, and
        // `found_new_run` overwrites the top's power before it can ever be read
        // (only the second-from-top run's power is inspected), so this `0` is a
        // placeholder that is never observed.
        self.pending[lower] = Run {
            base: left_run.base,
            len: left_run.len + right_run.len,
            power: 0,
        };
        self.pending.remove(lower + 1);

        // The left run's prefix that already precedes the right run's first
        // element, and the right run's suffix that already follows the left
        // run's last element, are in final position; skip them.
        let first_right = self.perm[right_run.base];
        let skip = self.gallop(
            first_right,
            Side::Perm,
            left_run.base,
            left_run.len,
            0,
            true,
        )?;
        let a_base = left_run.base + skip;
        let a_len = left_run.len - skip;
        if a_len == 0 {
            return Ok(());
        }
        let last_left = self.perm[a_base + a_len - 1];
        let b_len = self.gallop(
            last_left,
            Side::Perm,
            right_run.base,
            right_run.len,
            right_run.len - 1,
            false,
        )?;
        if b_len == 0 {
            return Ok(());
        }

        // Merge into whichever side needs the smaller temporary buffer.
        if a_len <= b_len {
            self.merge_lo(a_base, a_len, b_len)
        } else {
            self.merge_hi(a_base, a_len, b_len)
        }
    }

    /// Powersort policy for a newly identified run of length `new_run_len`:
    /// merge stacked runs whose boundary power exceeds the new boundary's, then
    /// record the new power on the current top. The caller pushes the new run.
    fn found_new_run(&mut self, new_run_len: usize) -> VmResult<()> {
        if self.pending.is_empty() {
            return Ok(());
        }
        let top = self.pending[self.pending.len() - 1];
        let power = node_power(top.base, top.len, new_run_len, self.perm.len());
        while self.pending.len() > 1 && self.pending[self.pending.len() - 2].power > power {
            let lower = self.pending.len() - 2;
            self.merge_at(lower)?;
        }
        let top_index = self.pending.len() - 1;
        self.pending[top_index].power = power;
        Ok(())
    }

    /// Merge every remaining run once no more runs will be found.
    fn merge_force_collapse(&mut self) -> VmResult<()> {
        while self.pending.len() > 1 {
            let count = self.pending.len();
            let lower = if count >= 3 && self.pending[count - 3].len < self.pending[count - 1].len {
                count - 3
            } else {
                count - 2
            };
            self.merge_at(lower)?;
        }
        Ok(())
    }

    /// Sort `perm` left to right: find each natural run, boost it to the minimum
    /// run length, apply the powersort merge policy, then drain the stack.
    fn run(&mut self) -> VmResult<()> {
        let len = self.perm.len();
        debug_assert!(
            self.keys_base + len <= self.vm.stack.len(),
            "the key region is rooted on the stack"
        );
        if len < 2 {
            return Ok(());
        }
        let min_run = merge_compute_minrun(len);
        let mut run_start = 0;
        while run_start < len {
            let remaining = len - run_start;
            let (mut run_len, descending) = self.count_run(run_start, remaining)?;
            if descending {
                self.perm[run_start..run_start + run_len].reverse();
            }
            if run_len < min_run {
                let boosted_len = min_run.min(remaining);
                self.binary_insertion_sort(run_start, boosted_len, run_len)?;
                run_len = boosted_len;
            }
            self.found_new_run(run_len)?;
            self.pending.push(Run {
                base: run_start,
                len: run_len,
                power: 0,
            });
            run_start += run_len;
        }
        self.merge_force_collapse()
    }
}

/// Split the optional ordering arguments of `sort` and `sorted` into a key
/// function and a reverse flag. A lone `bool` is the reverse flag; a lone
/// non-bool is the key. Two arguments are `(reverse, key)`. A `nil` key is
/// rejected: omit the key rather than passing `nil`.
fn parse_sort_args(vm: &mut VM, args: &[Value]) -> VmResult<(Option<Value>, bool)> {
    let (key, reverse) = match args {
        [] => (None, false),
        [Value::Bool(reverse)] => (None, *reverse),
        [key] => (Some(*key), false),
        [reverse, key] => {
            let Value::Bool(reverse) = reverse else {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "`sort` expects a boolean reverse flag, got `{}`.",
                            reverse.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            };
            (Some(*key), *reverse)
        }
        _ => {
            return Err(vm
                .throw(TypeError, "`sort` takes at most two arguments.")
                .unwrap_err());
        }
    };
    if matches!(key, Some(Value::Nil)) {
        return Err(vm
            .throw(
                TypeError,
                "A nil key can not be used to sort; omit the key instead.",
            )
            .unwrap_err());
    }
    Ok((key, reverse))
}

/// Sort the list in place. `list.sort()` orders by the elements themselves; a
/// single `bool` argument reverses the order; a single callable sorts by
/// `key(element)`; two arguments are `(reverse, key)`. The sort is stable and
/// returns nil.
///
/// The elements are moved out of the list for the duration, so a comparator
/// cannot disturb the region being sorted; a comparator or key that refills
/// the list raises `ValueError` once the sort finishes. Whatever the outcome,
/// the list is left holding a valid permutation of its original elements -
/// sorted on success, in some unspecified order on failure - and the write
/// below is the only place it is written.
pub(super) fn list_sort_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let (key, reverse) = parse_sort_args(vm, args)?;

    let items = std::mem::take(&mut receiver.as_list_mut(&mut vm.heap).items);
    let (values, outcome) = sort_values(vm, items, key, reverse);

    // The list was emptied for the sort; if a comparison or key call refilled
    // it, the user mutated it mid-sort. Detect this before the write below.
    let modified = outcome.is_ok() && !receiver.as_list(&vm.heap).items.is_empty();

    receiver.as_list_mut(&mut vm.heap).items = values;

    match outcome {
        Err(error) => Err(error),
        Ok(()) if modified => Err(vm
            .throw(ValueError, "List was modified during sorting.")
            .unwrap_err()),
        Ok(()) => Ok(Value::Nil),
    }
}

/// Return a new sorted list from any iterable: `sorted(iterable)` plus the
/// same optional ordering arguments as `list.sort` - `sorted(iterable,
/// reverse)`, `sorted(iterable, key)`, `sorted(iterable, reverse, key)`. The
/// input is iterated once up front and left untouched; the sort is stable.
pub(super) fn sorted_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let (key, reverse) = parse_sort_args(vm, &args[1..])?;
    let Some(items) = vm.collect_items_from_iterable(args[0])? else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected an iterable, got `{}`.",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };

    let (values, outcome) = sort_values(vm, items, key, reverse);
    outcome?;

    // Building the list does not re-enter the interpreter, so the unrooted
    // `values` cannot be collected before the instance takes ownership.
    let instance = Instance::new(
        *vm.heap.native_classes.get("List").unwrap(),
        Some(List::new(values).into()),
    );
    Ok(vm.heap.add_instance(instance))
}

/// Stably sort `items`, consuming them and returning the reordered values
/// alongside the outcome: on `Ok` they are sorted, on `Err` they are some
/// permutation of the input (nothing lost or duplicated). The values (and any
/// computed keys) are rooted on the VM stack for the duration, so re-entrant
/// comparisons and key calls cannot collect them; the stack is restored to its
/// entry depth before returning, keeping only the pending exception on top on
/// failure.
fn sort_values(
    vm: &mut VM,
    items: Vec<Value>,
    key: Option<Value>,
    reverse: bool,
) -> (Vec<Value>, VmResult<()>) {
    let base = vm.stack.len();
    let len = items.len();
    vm.stack.extend(items);

    let (perm, outcome) = run_sort(vm, key, reverse, base, len);

    // Materializing reads the rooted region without re-entering the
    // interpreter, so no collection can run here.
    let values: Vec<Value> = perm.iter().map(|&index| vm.stack[base + index]).collect();

    // Unwind the rooted region, keeping any pending exception on top.
    if matches!(outcome, Err(VmErrorKind::Exception(_))) {
        let exception = vm.stack.pop().expect("pending exception on stack top");
        vm.stack.truncate(base);
        vm.stack.push(exception);
    } else {
        vm.stack.truncate(base);
    }
    (values, outcome)
}

/// Decorate with keys if requested, then build the permutation and powersort
/// it. The elements already sit at `stack[items_base .. items_base+len]`.
/// Returns the permutation alongside the outcome; the permutation is always a
/// valid rearrangement of `0..len` (fully sorted on `Ok`, partially reordered
/// on `Err`), so the caller can materialize a valid result either way.
fn run_sort(
    vm: &mut VM,
    key: Option<Value>,
    reverse: bool,
    items_base: usize,
    len: usize,
) -> (Vec<usize>, VmResult<()>) {
    // Keys are computed before any reordering, so a failing key reports the
    // identity permutation and the values keep their original order.
    let keys_base = match compute_keys(vm, key, items_base, len) {
        Ok(keys_base) => keys_base,
        Err(error) => return ((0..len).collect(), Err(error)),
    };

    // Reverse via reverse-sort-reverse so equal elements keep their order.
    let perm: Vec<usize> = if reverse {
        (0..len).rev().collect()
    } else {
        (0..len).collect()
    };

    let mut sorter = Sorter {
        vm,
        keys_base,
        perm,
        scratch: Vec::with_capacity(len),
        pending: Vec::new(),
        min_gallop: MIN_GALLOP,
    };
    let outcome = sorter.run();
    let mut perm = sorter.perm;
    if outcome.is_ok() && reverse {
        perm.reverse();
    }
    (perm, outcome)
}

/// Compute `key(element)` for each of the `len` elements rooted at
/// `stack[items_base..]`, pushing the results as a second rooted region, and
/// return the offset of the first key. Without a key function the elements
/// order by themselves, so their own region is returned. On error the partial
/// key region stays on the stack, below the pending exception, for the caller
/// to unwind.
fn compute_keys(vm: &mut VM, key: Option<Value>, items_base: usize, len: usize) -> VmResult<usize> {
    debug_assert!(
        items_base + len <= vm.stack.len(),
        "the element region is rooted on the stack"
    );
    let Some(key) = key else {
        return Ok(items_base);
    };
    let keys_base = vm.stack.len();
    for offset in 0..len {
        let item = vm.stack[items_base + offset];
        vm.stack.push(key);
        vm.stack.push(item);
        vm.call_value_and_run(1)?;
    }
    Ok(keys_base)
}

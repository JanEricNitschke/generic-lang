//! Collection of small utility types.

// Should probably move all of the other Shrinkwrapped types in here as well.

use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug, PartialOrd)]
#[shrinkwrap(mutable)]
pub struct Line(pub usize);

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug, PartialOrd)]
#[shrinkwrap(mutable)]
pub struct Column(pub usize);

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd)]
pub struct Location {
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
    pub fn merge_ordered(&self, other: &Self) -> Self {
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

#[derive(Clone, Debug, Copy, PartialEq, Eq, Default)]
pub struct OpcodeLocation {
    pub(super) preceding: Option<Location>,
    pub(super) source: Location,
    pub(super) following: Option<Location>,
}

impl OpcodeLocation {
    pub fn new(source: Location) -> Self {
        Self {
            preceding: None,
            source,
            following: None,
        }
    }

    /// Returns a version where preceding/following are extended to touch the source
    pub fn filled(&self) -> Self {
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
pub enum Mutability {
    Mutable,
    Immutable,
}

/// Enum for conditional statement types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConditionType {
    If,
    Unless,
}

/// Enum for loop statement types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoopType {
    While,
    Until,
}

/// Enum for conditional jump directions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpCondition {
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
pub enum NumberEncoding {
    Short,
    Long,
}

/// Enum for function return modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnMode {
    Normal,
    Raw,
}

/// Enum for collection types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    Dict,
    Set,
}

/// Enum for equality comparison modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualityMode {
    Equal,
    NotEqual,
}

/// Enum for range boundary types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeType {
    Inclusive,
    Exclusive,
}

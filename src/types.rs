//! Collection of small utility types.

// Should probably move all of the other Shrinkwrapped types in here as well.

use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug)]
#[shrinkwrap(mutable)]
pub struct Line(pub usize);

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

/// Enum for number encoding types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberEncoding {
    Short,
    Long,
}

/// Enum for function return types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnType {
    Normal,
    Raw,
}

/// Enum for collection types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    Dict,
    Set,
}

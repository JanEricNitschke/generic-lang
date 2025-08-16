//! Dedicated enums to replace boolean parameters in function signatures.
//! 
//! This module contains enums that provide better type safety and clarity
//! compared to raw boolean parameters.

/// Enum for controlling conditional jump operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpCondition {
    /// Jump if the condition is true
    IfTrue,
    /// Jump if the condition is false (or falsey)
    IfFalse,
}

impl From<bool> for JumpCondition {
    fn from(value: bool) -> Self {
        if value {
            Self::IfTrue
        } else {
            Self::IfFalse
        }
    }
}

impl From<JumpCondition> for bool {
    fn from(condition: JumpCondition) -> Self {
        matches!(condition, JumpCondition::IfTrue)
    }
}

/// Enum for controlling variable mutability.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    /// Variable can be modified after declaration
    Mutable,
    /// Variable cannot be modified after declaration
    Immutable,
}

impl From<bool> for Mutability {
    fn from(value: bool) -> Self {
        if value {
            Self::Mutable
        } else {
            Self::Immutable
        }
    }
}

impl From<Mutability> for bool {
    fn from(mutability: Mutability) -> Self {
        matches!(mutability, Mutability::Mutable)
    }
}

/// Enum for controlling assignment capability in parser functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentCapability {
    /// Expression can be assigned to
    CanAssign,
    /// Expression cannot be assigned to
    CannotAssign,
}

impl From<bool> for AssignmentCapability {
    fn from(value: bool) -> Self {
        if value {
            Self::CanAssign
        } else {
            Self::CannotAssign
        }
    }
}

impl From<AssignmentCapability> for bool {
    fn from(capability: AssignmentCapability) -> Self {
        matches!(capability, AssignmentCapability::CanAssign)
    }
}

/// Enum for distinguishing different statement types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum StatementType {
    /// If statement
    If,
    /// While loop statement
    While,
}

/// Enum for distinguishing loop types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum LoopType {
    /// While loop
    While,
    /// For loop
    For,
}

/// Enum for controlling import types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum ImportType {
    /// Local import (relative path)
    Local,
    /// Global import (absolute path)
    Global,
}

impl From<bool> for ImportType {
    fn from(value: bool) -> Self {
        if value {
            Self::Local
        } else {
            Self::Global
        }
    }
}

impl From<ImportType> for bool {
    fn from(import_type: ImportType) -> Self {
        matches!(import_type, ImportType::Local)
    }
}

/// Enum for controlling number/constant sizes in bytecode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstantSize {
    /// Use long (24-bit) constant encoding
    Long,
    /// Use short (8-bit) constant encoding
    Short,
}

impl From<bool> for ConstantSize {
    fn from(value: bool) -> Self {
        if value {
            Self::Long
        } else {
            Self::Short
        }
    }
}

impl From<ConstantSize> for bool {
    fn from(size: ConstantSize) -> Self {
        matches!(size, ConstantSize::Long)
    }
}

/// Enum for distinguishing collection types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    /// Dictionary/map collection
    Dictionary,
    /// Set collection
    Set,
}

impl From<bool> for CollectionType {
    fn from(value: bool) -> Self {
        if value {
            Self::Dictionary
        } else {
            Self::Set
        }
    }
}

impl From<CollectionType> for bool {
    fn from(collection_type: CollectionType) -> Self {
        matches!(collection_type, CollectionType::Dictionary)
    }
}

/// Enum for distinguishing class types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum ClassType {
    /// Native (built-in) class
    Native,
    /// User-defined class
    UserDefined,
}

impl From<bool> for ClassType {
    fn from(value: bool) -> Self {
        if value {
            Self::Native
        } else {
            Self::UserDefined
        }
    }
}

impl From<ClassType> for bool {
    fn from(class_type: ClassType) -> Self {
        matches!(class_type, ClassType::Native)
    }
}

/// Enum for controlling range types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeType {
    /// Exclusive range (end value not included)
    Exclusive,
    /// Inclusive range (end value included)
    Inclusive,
}

impl From<bool> for RangeType {
    fn from(value: bool) -> Self {
        if value {
            Self::Exclusive
        } else {
            Self::Inclusive
        }
    }
}

impl From<RangeType> for bool {
    fn from(range_type: RangeType) -> Self {
        matches!(range_type, RangeType::Exclusive)
    }
}

/// Enum for controlling equality operation types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualityOperation {
    /// Standard equality
    Equal,
    /// Negated equality (not equal)
    NotEqual,
}

impl From<bool> for EqualityOperation {
    fn from(value: bool) -> Self {
        if value {
            Self::NotEqual
        } else {
            Self::Equal
        }
    }
}

impl From<EqualityOperation> for bool {
    fn from(operation: EqualityOperation) -> Self {
        matches!(operation, EqualityOperation::NotEqual)
    }
}

/// Enum for controlling compilation end modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilationEndMode {
    /// Raw compilation end (no additional processing)
    Raw,
    /// Standard compilation end (with additional processing)
    Standard,
}

impl From<bool> for CompilationEndMode {
    fn from(value: bool) -> Self {
        if value {
            Self::Raw
        } else {
            Self::Standard
        }
    }
}

impl From<CompilationEndMode> for bool {
    fn from(mode: CompilationEndMode) -> Self {
        matches!(mode, CompilationEndMode::Raw)
    }
}

/// Enum for distinguishing upvalue types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpvalueType {
    /// Local upvalue (captures local variable)
    Local,
    /// Nested upvalue (captures upvalue from enclosing scope)
    Nested,
}

impl From<bool> for UpvalueType {
    fn from(value: bool) -> Self {
        if value {
            Self::Local
        } else {
            Self::Nested
        }
    }
}

impl From<UpvalueType> for bool {
    fn from(upvalue_type: UpvalueType) -> Self {
        matches!(upvalue_type, UpvalueType::Local)
    }
}
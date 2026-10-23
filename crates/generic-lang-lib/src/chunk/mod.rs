//! Contains the `OpCode` enum as well as the chunks containing the bytecode to be interpreted.

#[cfg(any(feature = "trace_execution", feature = "print_code"))]
mod disassemble;
#[cfg(feature = "trace_execution")]
pub(crate) use disassemble::InstructionDisassembler;

use crate::{heap::StringId, types::OpcodeLocation, value::Value};
use derivative::Derivative;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use shrinkwraprs::Shrinkwrap;
use strum_macros::{AsRefStr, EnumIter};
#[derive(Shrinkwrap, Clone, Copy, Debug)]
#[shrinkwrap(mutable)]
pub(crate) struct CodeOffset(pub usize);

#[derive(Shrinkwrap, Clone, Copy, Debug)]
pub(crate) struct ConstantIndex(pub u8);

impl From<ConstantIndex> for u8 {
    fn from(index: ConstantIndex) -> Self {
        index.0
    }
}

#[derive(Shrinkwrap, Clone, Copy, Debug)]
pub(crate) struct ConstantLongIndex(pub usize);

impl TryFrom<ConstantLongIndex> for ConstantIndex {
    type Error = <u8 as TryFrom<usize>>::Error;

    fn try_from(value: ConstantLongIndex) -> Result<Self, Self::Error> {
        let short = u8::try_from(value.0)?;
        Ok(Self(short))
    }
}

/// The set of `OpCodes` emitted by the compiler to be interpreted/executed by the VM.
#[derive(
    IntoPrimitive, TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy, EnumIter, AsRefStr,
)]
#[repr(u8)]
pub(crate) enum OpCode {
    // Literals and constants.
    Nil,
    True,
    False,
    StopIteration,
    LoadZero,
    LoadOne,
    LoadTwo,
    LoadMinusOne,
    LoadZerof,
    LoadOnef,
    Constant,
    ConstantLong,

    // Stack manipulation.
    Pop,
    Dup,
    DupN,
    Swap,

    // Variables: locals, globals, upvalues.
    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,
    GetGlobal,
    GetGlobalLong,
    SetGlobal,
    SetGlobalLong,
    DefineGlobal,
    DefineGlobalLong,
    DefineGlobalConst,
    DefineGlobalConstLong,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,

    // Control flow.
    Jump,
    JumpIfNil,
    JumpIfFalse,
    JumpIfTrue,
    PopJumpIfFalse,
    PopJumpIfTrue,
    JumpIfFalseOrPop,
    JumpIfTrueOrPop,
    Loop,

    // Calls and returns.
    Closure,
    Call,
    CallUnpack,
    Invoke,
    InvokeUnpack,
    SuperInvoke,
    SuperInvokeUnpack,
    Return,
    ReturnGenerator,
    Yield,

    // Unary, arithmetic, and bit operators.
    Negate,
    Not,
    Add,
    Subtract,
    Multiply,
    Divide,
    FloorDiv,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,

    // Comparison.
    Is,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Classes and properties.
    Class,
    Inherit,
    Method,
    ClassVariable,
    GetProperty,
    SetProperty,
    GetSuper,

    // Collection, numeric, and string builders.
    BuildList,
    BuildTuple,
    BuildSet,
    BuildDict,
    BuildRational,
    BuildRangeExclusive,
    BuildRangeInclusive,
    BuildFstring,
    BuildInterpolation,
    BuildTemplate,

    // Imports.
    Import,
    ImportAs,
    ImportFrom,

    // Exception handling.
    RegisterCatches,
    PopHandler,
    CompareException,
    Throw,
    Reraise,
}

#[cfg(test)]
#[test]
fn test_opcode_size() {
    assert_eq!(size_of::<OpCode>(), 1);
}

impl OpCode {
    pub(super) const fn to_long(self) -> Self {
        match self {
            Self::GetLocal => Self::GetLocalLong,
            Self::GetGlobal => Self::GetGlobalLong,
            Self::SetLocal => Self::SetLocalLong,
            Self::SetGlobal => Self::SetGlobalLong,
            Self::DefineGlobal => Self::DefineGlobalLong,
            Self::DefineGlobalConst => Self::DefineGlobalConstLong,
            x => x,
        }
    }
}

/// Wraps block of bytecode used for interpretation.
///
/// Each main script, module and function has its own `Chunk`.
/// Each chunk has a name, mainly for debugging purposes, its code,
/// location information for each entry in the code array as well as a constant
/// table for literal constants that appear in the chunk.
#[derive(Derivative, Clone, Debug)]
#[derivative(PartialEq)]
pub(crate) struct Chunk {
    name: StringId,
    code: Vec<u8>,
    #[derivative(PartialEq = "ignore")]
    locations: Vec<OpcodeLocation>,
    #[derivative(PartialEq = "ignore")]
    constants: Vec<Value>,
}
impl Eq for Chunk {}

impl Chunk {
    pub(super) fn new(name: StringId) -> Self {
        Self {
            name,
            code: Vec::default(),
            locations: Vec::default(),
            constants: Vec::default(),
        }
    }

    pub(super) fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub(super) fn name(&self) -> StringId {
        self.name
    }

    pub(super) fn code(&self) -> &[u8] {
        &self.code
    }

    /// Retrieve a constant by index.
    pub(super) fn get_constant<T>(&self, index: T) -> &Value
    where
        T: Into<usize>,
    {
        &self.constants[index.into()]
    }

    /// Write a byte (`OpCode` or operand) into the chunk.
    /// Also update the location information accordingly.
    pub(super) fn write<T>(&mut self, what: T, location: OpcodeLocation)
    where
        T: Into<u8>,
    {
        self.code.push(what.into());
        self.locations.push(location);
    }

    /// Patch an existing entry in the code.
    ///
    /// Mainly used for patching forward jumps in conditions
    /// where the size of the code to jump over is not yet known.
    pub(super) fn patch<T>(&mut self, offset: CodeOffset, what: T)
    where
        T: Into<u8>,
    {
        self.code[*offset] = what.into();
    }

    /// Add a constant to the constant table and return its index.
    pub(super) fn make_constant(&mut self, what: Value) -> ConstantLongIndex {
        self.constants.push(what);
        ConstantLongIndex(self.constants.len() - 1)
    }

    /// Write a constant into the code.
    /// Create it in the constant table and write the index preceded by
    /// the corresponding `OpCode`.
    pub(super) fn write_constant(&mut self, what: Value, location: OpcodeLocation) -> bool {
        let long_index = self.make_constant(what);
        if let Ok(short_index) = u8::try_from(*long_index) {
            self.write(OpCode::Constant, location);
            self.write(short_index, location);
            true
        } else {
            self.write(OpCode::ConstantLong, location);
            self.write_24bit_number(*long_index, location)
        }
    }

    pub(super) fn write_24bit_number(&mut self, what: usize, location: OpcodeLocation) -> bool {
        let (a, b, c, d) = crate::bitwise::get_4_bytes(what);
        if a > 0 {
            return false;
        }
        self.write(b, location);
        self.write(c, location);
        self.write(d, location);
        true
    }

    pub(super) fn get_location(&self, offset: CodeOffset) -> OpcodeLocation {
        self.locations[*offset.as_ref()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{heap::Heap, value::Value};

    fn create_test_heap() -> Heap {
        Heap::new()
    }

    fn create_test_chunk(heap: &mut Heap) -> Chunk {
        let name_val = heap.add_string("test_chunk".to_string());
        let name_id = *name_val.as_string();
        Chunk::new(name_id)
    }

    #[test]
    fn test_constant_management() {
        let mut heap = create_test_heap();
        let mut chunk = create_test_chunk(&mut heap);

        let index1 = chunk.make_constant(Value::Bool(true));
        let index2 = chunk.make_constant(Value::Bool(false));

        assert_eq!(*index1, 0);
        assert_eq!(*index2, 1);
        assert_eq!(chunk.constants().len(), 2);
        assert_eq!(*chunk.get_constant(0usize), Value::Bool(true));
        assert_eq!(*chunk.get_constant(1usize), Value::Bool(false));
    }

    #[test]
    fn test_code_patching() {
        let mut heap = create_test_heap();
        let mut chunk = create_test_chunk(&mut heap);

        chunk.write(OpCode::Jump, OpcodeLocation::default());
        let patch_offset = CodeOffset(chunk.code().len());
        chunk.write(0u8, OpcodeLocation::default()); // Placeholder for jump offset

        // Patch the jump offset
        chunk.patch(patch_offset, 42u8);
        assert_eq!(chunk.code()[1], 42);
    }

    #[test]
    fn test_24bit_number_handling() {
        let mut heap = create_test_heap();
        let mut chunk = create_test_chunk(&mut heap);

        // Test valid 24-bit number
        let result = chunk.write_24bit_number(0x0012_3456, OpcodeLocation::default());
        assert!(result);
        assert_eq!(chunk.code().len(), 3);
        assert_eq!(chunk.code()[0], 0x12);
        assert_eq!(chunk.code()[1], 0x34);
        assert_eq!(chunk.code()[2], 0x56);

        // Test number too large for 24-bit
        let mut chunk2 = create_test_chunk(&mut heap);
        let result2 = chunk2.write_24bit_number(0x0100_0000, OpcodeLocation::default()); // Requires 25 bits
        assert!(!result2); // Should fail
        assert_eq!(chunk2.code().len(), 0); // Nothing should be written
    }
}

//! Contains the `OpCode` enum as well as the chunks containing the bytecode to be interpreted.

#![allow(dead_code)]

use crate::config::LAMBDA_NAME;
use crate::heap::Heap;
use crate::{heap::StringId, types::Location, value::Value};
use convert_case::{Case, Casing};
use derivative::Derivative;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use paste::paste;
use shrinkwraprs::Shrinkwrap;
use std::fmt::Debug;
use strum::IntoEnumIterator;
use strum_macros::{AsRefStr, EnumIter};
#[derive(Shrinkwrap, Clone, Copy, Debug)]
#[shrinkwrap(mutable)]
pub struct CodeOffset(pub usize);

#[derive(Shrinkwrap, Clone, Copy)]
pub struct ConstantIndex(pub u8);

impl From<ConstantIndex> for u8 {
    fn from(index: ConstantIndex) -> Self {
        index.0
    }
}

#[derive(Shrinkwrap, Clone, Copy)]
pub struct ConstantLongIndex(pub usize);

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
pub enum OpCode {
    Constant,
    ConstantLong,

    DefineGlobal,
    DefineGlobalLong,
    DefineGlobalConst,
    DefineGlobalConstLong,

    GetGlobal,
    GetGlobalLong,
    SetGlobal,
    SetGlobalLong,

    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,

    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,

    Jump,
    JumpIfFalse,
    JumpIfTrue,
    PopJumpIfFalse,
    PopJumpIfTrue,
    JumpIfTrueOrPop,
    JumpIfFalseOrPop,
    Loop,
    Call,

    Nil,
    True,
    False,
    StopIteration,

    Equal,
    NotEqual,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    LoadOne,
    LoadTwo,
    LoadZero,
    LoadOnef,
    LoadZerof,
    LoadMinusOne,
    // CompZero
    // AddOne
    // SubOne
    Negate,
    Not,

    Add,
    Subtract,
    Multiply,
    Divide,
    BitXor,
    BitOr,
    BitAnd,
    Mod,
    Exp,
    FloorDiv,

    Pop,
    Dup,
    DupN,
    Swap,

    Invoke,
    Method,
    Closure,
    Return,

    Class,
    SetProperty,
    GetProperty,
    Inherit,
    GetSuper,
    SuperInvoke,

    BuildList,
    BuildTuple,
    BuildSet,
    BuildDict,
    BuildRational,
    BuildRangeExclusive,
    BuildRangeInclusive,
    BuildFstring,

    Import,
    ImportFrom,
    ImportAs,

    RegisterCatches,
    PopHandler,
    CompareException,
    Reraise,
    Throw,
}

#[cfg(test)]
#[test]
fn test_opcode_size() {
    assert_eq!(std::mem::size_of::<OpCode>(), 1);
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

    /// Get the length of the longest `OpCode` when turned into snake case.
    ///
    /// Used for aligning debugging output.
    fn max_name_length() -> usize {
        Self::iter()
            .map(|v| v.as_ref().to_case(Case::UpperSnake).len())
            .max()
            .unwrap_or(0)
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
pub struct Chunk {
    name: StringId,
    code: Vec<u8>,
    #[derivative(PartialEq = "ignore")]
    locations: Vec<Location>,
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
    pub(super) fn write<T>(&mut self, what: T, location: Location)
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
    pub(super) fn write_constant(&mut self, what: Value, location: Location) -> bool {
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

    pub(super) fn write_24bit_number(&mut self, what: usize, location: Location) -> bool {
        let (a, b, c, d) = crate::bitwise::get_4_bytes(what);
        if a > 0 {
            return false;
        }
        self.write(b, location);
        self.write(c, location);
        self.write(d, location);
        true
    }

    pub(super) fn get_location(&self, offset: CodeOffset) -> Location {
        self.locations[*offset.as_ref()]
    }
}

impl Chunk {
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        let name = self.name.to_value(heap);

        let mut result = if name == LAMBDA_NAME {
            format!(
                "== {name} ({:?}) ==\n",
                self.locations.first().map(|loc| loc.end_line)
            )
        } else {
            format!("== {name} ==\n")
        };

        let mut disassembler = InstructionDisassembler::new(self, heap);
        while disassembler.offset.as_ref() < &self.code.len() {
            let disasm_output = format!("{disassembler:?}");
            result.push_str(&disasm_output);
            *disassembler.offset += disassembler.instruction_len(*disassembler.offset, heap);
        }

        result
    }
}

/// Debug helper for disassembling a chunks code into
/// a human readable format.
pub struct InstructionDisassembler<'chunk, 'heap> {
    chunk: &'chunk Chunk,
    pub(super) offset: CodeOffset,
    operand_alignment: usize,
    opcode_name_alignment: usize,
    heap: &'heap Heap,
}

impl<'chunk, 'heap> InstructionDisassembler<'chunk, 'heap> {
    #[must_use]
    pub(super) fn new(chunk: &'chunk Chunk, heap: &'heap Heap) -> Self {
        Self {
            chunk,
            offset: CodeOffset(0),
            operand_alignment: 4,
            // +3 because we add "OP_" to the start.
            opcode_name_alignment: OpCode::max_name_length() + 3,
            heap,
        }
    }

    #[allow(clippy::enum_glob_use)]
    fn instruction_len(&self, offset: usize, heap: &Heap) -> usize {
        use OpCode::*;
        let opcode = OpCode::try_from_primitive(self.chunk.code[offset]).unwrap();
        std::mem::size_of::<OpCode>()
            + match opcode {
                Negate | Add | Subtract | Multiply | Divide | Mod | Exp | FloorDiv | BitAnd
                | BitOr | BitXor | BuildRational | BuildRangeExclusive | BuildRangeInclusive
                | Nil | True | False | StopIteration | Not | Equal | Greater | Less | LessEqual
                | GreaterEqual | NotEqual | Pop | Dup | CloseUpvalue | Inherit | Return
                | LoadOne | LoadTwo | LoadZero | LoadMinusOne | LoadOnef | LoadZerof | Swap
                | PopHandler | CompareException | Throw | Reraise => 0,
                Constant | GetLocal | SetLocal | GetGlobal | SetGlobal | DefineGlobal
                | DefineGlobalConst | Call | GetUpvalue | SetUpvalue | Class | GetProperty
                | SetProperty | Method | GetSuper | BuildList | BuildTuple | BuildSet
                | BuildDict | BuildFstring | DupN => 1,
                Jump | JumpIfFalse | JumpIfTrue | PopJumpIfFalse | PopJumpIfTrue
                | JumpIfTrueOrPop | JumpIfFalseOrPop | RegisterCatches | Loop | Invoke | Import
                | SuperInvoke => 2,
                ConstantLong
                | GetGlobalLong
                | SetGlobalLong
                | DefineGlobalLong
                | DefineGlobalConstLong
                | GetLocalLong
                | SetLocalLong
                | ImportAs => 3,
                Closure => 1 + self.upvalue_code_len(offset, heap),
                ImportFrom => 2 + self.import_from_len(offset),
            }
    }

    fn upvalue_code_len(&self, closure_offset: usize, heap: &Heap) -> usize {
        let code = self.chunk.code();
        let constant = code[closure_offset + 1];
        let value = self.chunk.get_constant(constant);
        value.as_function().to_value(heap).upvalue_count * 2
    }

    fn import_from_len(&self, import_from_offset: usize) -> usize {
        let code = self.chunk.code();
        let n_op = code[import_from_offset + 3];
        1 + n_op as usize
    }

    fn debug_constant_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let constant_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 1]);
        let constant_value = if (*constant_index.as_ref() as usize) < self.chunk.constants.len() {
            *self.chunk.get_constant(*constant_index.as_ref())
        } else {
            Value::Nil // self.chunk.name.into()
        };
        write!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$}",
            name,
            *constant_index,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;
        writeln!(f, " '{}'  ", constant_value.to_string(heap))
    }

    fn debug_import_standard_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let import_path_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 1]);
        let is_local = self.chunk.code()[offset.as_ref() + 2] == 1;
        write!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$}",
            name,
            *import_path_index,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;
        writeln!(
            f,
            " '{}' | '{}'",
            self.chunk
                .get_constant(*import_path_index.as_ref())
                .to_string(heap),
            if is_local { "local" } else { "global" }
        )
    }

    fn debug_import_as_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let import_path_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 1]);
        let alias_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 2]);
        let is_local = self.chunk.code()[offset.as_ref() + 3] == 1;
        write!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$}",
            name,
            *import_path_index,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;
        writeln!(
            f,
            " '{}' | {} '{}'  |  '{}'",
            self.chunk
                .get_constant(*import_path_index.as_ref())
                .to_string(heap),
            *alias_index,
            self.chunk
                .get_constant(*alias_index.as_ref())
                .to_string(heap),
            if is_local { "local" } else { "global" }
        )
    }

    fn debug_import_from_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let import_path_index = ConstantIndex(code[offset.as_ref() + 1]);
        let is_local = code[offset.as_ref() + 2] == 1;
        let n_operands = code[offset.as_ref() + 3];
        write!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$}",
            name,
            *import_path_index,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;
        write!(
            f,
            " '{}' | '{}'",
            self.chunk
                .get_constant(*import_path_index.as_ref())
                .to_string(heap),
            if is_local { "local" } else { "global" }
        )?;
        for op_idx in 0..n_operands {
            let name_index = ConstantIndex(code[offset.as_ref() + op_idx as usize + 4]);
            let name = *self.chunk.get_constant(*name_index.as_ref());

            write!(f, " | {} '{}'", *name_index, name.to_string(heap))?;
        }
        writeln!(f)?;
        Ok(())
    }

    fn debug_constant_long_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant_index = ConstantLongIndex(
            (usize::from(code[offset.as_ref() + 1]) << 16)
                + (usize::from(code[offset.as_ref() + 2]) << 8)
                + (usize::from(code[offset.as_ref() + 3])),
        );
        writeln!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$} '{}'",
            name,
            *constant_index,
            self.chunk
                .get_constant(*constant_index.as_ref())
                .to_string(heap),
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }

    #[allow(clippy::unused_self)]
    fn debug_simple_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        _offset: CodeOffset,
        _heap: &Heap,
    ) -> std::fmt::Result {
        writeln!(f, "{name}")
    }

    fn debug_byte_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        _heap: &Heap,
    ) -> std::fmt::Result {
        let slot = self.chunk.code[*offset + 1];
        writeln!(
            f,
            "{name:-OPCODE_NAME_ALIGNMENT$} {slot:>OPERAND_ALIGNMENT$}",
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }

    fn debug_byte_long_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        _heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let slot = (usize::from(code[offset.as_ref() + 1]) << 16)
            + (usize::from(code[offset.as_ref() + 2]) << 8)
            + (usize::from(code[offset.as_ref() + 3]));
        writeln!(
            f,
            "{name:-OPCODE_NAME_ALIGNMENT$} {slot:>OPERAND_ALIGNMENT$}",
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }

    fn debug_jump_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let jump = (usize::from(code[offset.as_ref() + 1]) << 8)
            + (usize::from(code[offset.as_ref() + 2]));
        let target = *offset + self.instruction_len(*offset, heap);
        let target = if OpCode::try_from_primitive(code[*offset]).unwrap() == OpCode::Loop {
            target - jump
        } else {
            target + jump
        };
        writeln!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$} -> {}",
            name,
            *offset,
            target,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }

    fn debug_closure_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let mut offset = *offset + 1;

        let code = self.chunk.code();
        //eprintln!("{:?}", &code[offset..]);
        let constant = code[offset];
        offset += 1;

        let value = self.chunk.get_constant(constant);
        writeln!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$} {}",
            name,
            constant,
            value.to_string(heap),
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment,
        )?;

        let function = value.as_function();
        //eprintln!("{} {}", *function.name, function.upvalue_count);
        for _ in 0..function.to_value(heap).upvalue_count {
            let is_local = code[offset];
            offset += 1;

            debug_assert!(
                is_local == 0 || is_local == 1,
                "is_local must be 0 or 1, got: {is_local}"
            );
            let is_local = is_local == 1;

            let index = code[offset];
            offset += 1;
            writeln!(
                f,
                "{:04}    |{} {} {}",
                offset - 2,
                // +1 for the space before opcode_name and 1 between name and operand
                " ".repeat(self.opcode_name_alignment + self.operand_alignment + 2),
                if is_local { "local" } else { "upvalue" },
                index
            )?;
        }

        Ok(())
    }

    fn debug_invoke_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant = code[offset.as_ref() + 1];
        let arg_count = code[offset.as_ref() + 2];
        let constant_value = self.chunk.get_constant(constant);
        let formatted_name = format!("{name} ({arg_count} args)");
        writeln!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$} '{}'",
            formatted_name,
            constant,
            constant_value.to_string(heap),
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }
}

macro_rules! disassemble {
    (
        $self:ident,
        $f:ident,
        $offset:ident,
        $heap:ident,
        $m:expr,
        $(
            $k:ident(
                $($v:ident),* $(,)?
            )
        ),* $(,)?
    ) => {paste! {
        match $m {
            $($(
                OpCode::$v => $self.[<debug_ $k _opcode>]($f, stringify!([<OP_ $v:snake:upper>]), $offset, $heap)
            ),*),*
        }
    }}
}

impl Debug for InstructionDisassembler<'_, '_> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self.chunk.code();
        let offset = self.offset;

        write!(f, "{:04} ", *offset.as_ref())?;
        if *offset.as_ref() > 0
            && self.chunk.get_location(offset)
                == self.chunk.get_location(CodeOffset(offset.as_ref() - 1))
        {
            write!(f, "   | ")?;
        } else {
            write!(
                f,
                "{:>OPERAND_ALIGNMENT$} ",
                *self.chunk.get_location(offset).end_line,
                OPERAND_ALIGNMENT = self.operand_alignment
            )?;
        }

        let opcode = OpCode::try_from_primitive(code[*offset.as_ref()])
            .unwrap_or_else(|_| panic!("Unknown opcode: {}", code[*offset.as_ref()]));

        let heap = &self.heap;
        disassemble!(
            self,
            f,
            offset,
            heap,
            opcode,
            constant(
                Constant,
                DefineGlobal,
                DefineGlobalConst,
                GetGlobal,
                SetGlobal,
                GetProperty,
                SetProperty,
                Method,
                GetSuper,
            ),
            constant_long(
                ConstantLong,
                DefineGlobalLong,
                DefineGlobalConstLong,
                GetGlobalLong,
                SetGlobalLong,
            ),
            import_standard(Import),
            import_as(ImportAs),
            import_from(ImportFrom),
            closure(Closure),
            byte(
                Call,
                GetUpvalue,
                SetUpvalue,
                Class,
                GetLocal,
                SetLocal,
                BuildList,
                BuildTuple,
                BuildSet,
                BuildDict,
                BuildFstring,
                DupN,
            ),
            byte_long(GetLocalLong, SetLocalLong),
            jump(
                Jump,
                JumpIfFalse,
                JumpIfTrue,
                PopJumpIfFalse,
                PopJumpIfTrue,
                JumpIfTrueOrPop,
                JumpIfFalseOrPop,
                Loop,
                RegisterCatches
            ),
            invoke(Invoke, SuperInvoke),
            simple(
                Add,
                CloseUpvalue,
                Divide,
                Dup,
                Equal,
                GreaterEqual,
                LessEqual,
                NotEqual,
                False,
                Greater,
                Inherit,
                Less,
                Multiply,
                BitAnd,
                BitOr,
                BitXor,
                Mod,
                Exp,
                FloorDiv,
                BuildRational,
                BuildRangeExclusive,
                BuildRangeInclusive,
                Negate,
                Nil,
                StopIteration,
                Not,
                Pop,
                Return,
                Subtract,
                True,
                LoadOne,
                LoadTwo,
                LoadZero,
                LoadMinusOne,
                LoadOnef,
                LoadZerof,
                Swap,
                PopHandler,
                CompareException,
                Throw,
                Reraise
            ),
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{heap::Heap, types::Location, value::Value};

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

        chunk.write(OpCode::Jump, Location::default());
        let patch_offset = CodeOffset(chunk.code().len());
        chunk.write(0u8, Location::default()); // Placeholder for jump offset

        // Patch the jump offset
        chunk.patch(patch_offset, 42u8);
        assert_eq!(chunk.code()[1], 42);
    }

    #[test]
    fn test_24bit_number_handling() {
        let mut heap = create_test_heap();
        let mut chunk = create_test_chunk(&mut heap);

        // Test valid 24-bit number
        let result = chunk.write_24bit_number(0x0012_3456, Location::default());
        assert!(result);
        assert_eq!(chunk.code().len(), 3);
        assert_eq!(chunk.code()[0], 0x12);
        assert_eq!(chunk.code()[1], 0x34);
        assert_eq!(chunk.code()[2], 0x56);

        // Test number too large for 24-bit
        let mut chunk2 = create_test_chunk(&mut heap);
        let result2 = chunk2.write_24bit_number(0x0100_0000, Location::default()); // Requires 25 bits
        assert!(!result2); // Should fail
        assert_eq!(chunk2.code().len(), 0); // Nothing should be written
    }
}

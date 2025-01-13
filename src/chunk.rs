//! Contains the `OpCode` enum as well as the chunks containing the bytecode to be interpreted.

use crate::{heap::StringId, types::Line, value::Value};
use convert_case::{Case, Casing};
use derivative::Derivative;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use paste::paste;
use shrinkwraprs::Shrinkwrap;
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
    BuildSet,
    BuildDict,

    Import,
    ImportFrom,
    ImportAs,
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
/// line information for each entry in the code array as well as a constant
/// table for literal constants that appear in the chunk.
#[derive(PartialEq, Eq, Derivative, Clone)]
#[derivative(PartialOrd)]
pub struct Chunk {
    name: StringId,
    code: Vec<u8>,
    #[derivative(PartialOrd = "ignore")]
    /// Lines are runlength encoded as there are usually
    /// multiple bytes that originate from the same line.
    lines: Vec<(usize, Line)>,
    constants: Vec<Value>,
}

impl Chunk {
    pub(super) fn new(name: StringId) -> Self {
        Self {
            name,
            code: Vec::default(),
            lines: Vec::default(),
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
    /// Also update the line information accordingly.
    pub(super) fn write<T>(&mut self, what: T, line: Line)
    where
        T: Into<u8>,
    {
        self.code.push(what.into());
        match self.lines.last_mut() {
            Some((count, last_line)) if last_line.as_ref() == line.as_ref() => {
                *count += 1;
            }
            _ => self.lines.push((1, line)),
        }
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
    pub(super) fn write_constant(&mut self, what: Value, line: Line) -> bool {
        let long_index = self.make_constant(what);
        if let Ok(short_index) = u8::try_from(*long_index) {
            self.write(OpCode::Constant, line);
            self.write(short_index, line);
            true
        } else {
            self.write(OpCode::ConstantLong, line);
            self.write_24bit_number(*long_index, line)
        }
    }

    pub(super) fn write_24bit_number(&mut self, what: usize, line: Line) -> bool {
        let (a, b, c, d) = crate::bitwise::get_4_bytes(what);
        if a > 0 {
            return false;
        }
        self.write(b, line);
        self.write(c, line);
        self.write(d, line);
        true
    }

    /// Decode the runlength encoded lines for a specific offset.
    pub(super) fn get_line(&self, offset: CodeOffset) -> Line {
        let mut iter = self.lines.iter();
        let (mut consumed, mut line) = iter.next().unwrap();
        while consumed <= *offset.as_ref() {
            let entry = iter.next().unwrap();
            consumed += entry.0;
            line = entry.1;
        }
        line
    }
}

impl std::fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== {} ==", *self.name)?;
        let mut disassembler = InstructionDisassembler::new(self);
        while disassembler.offset.as_ref() < &self.code.len() {
            write!(f, "{disassembler:?}")?;
            *disassembler.offset += disassembler.instruction_len(*disassembler.offset);
        }
        Ok(())
    }
}

/// Debug helper for disassembling a chunks code into
/// a human readable format.
pub struct InstructionDisassembler<'chunk> {
    chunk: &'chunk Chunk,
    pub(super) offset: CodeOffset,
    operand_alignment: usize,
    opcode_name_alignment: usize,
}

impl<'chunk> InstructionDisassembler<'chunk> {
    #[must_use]
    pub(super) fn new(chunk: &'chunk Chunk) -> Self {
        Self {
            chunk,
            offset: CodeOffset(0),
            operand_alignment: 4,
            // +3 because we add "OP_" to the start.
            opcode_name_alignment: OpCode::max_name_length() + 3,
        }
    }

    #[allow(clippy::enum_glob_use)]
    fn instruction_len(&self, offset: usize) -> usize {
        use OpCode::*;
        let opcode = OpCode::try_from_primitive(self.chunk.code[offset]).unwrap();
        std::mem::size_of::<OpCode>()
            + match opcode {
                Negate | Add | Subtract | Multiply | Divide | Mod | Exp | FloorDiv | BitAnd
                | BitOr | BitXor | Nil | True | False | StopIteration | Not | Equal | Greater
                | Less | LessEqual | GreaterEqual | NotEqual | Pop | Dup | CloseUpvalue
                | Inherit | Import | LoadOne | LoadTwo | LoadZero | LoadMinusOne | LoadOnef
                | LoadZerof | Swap => 0,
                Constant | GetLocal | SetLocal | GetGlobal | SetGlobal | DefineGlobal
                | DefineGlobalConst | Call | Return | GetUpvalue | SetUpvalue | Class
                | GetProperty | SetProperty | Method | GetSuper | BuildList | BuildSet
                | BuildDict | DupN | ImportFrom | ImportAs => 1,
                Jump | JumpIfFalse | JumpIfTrue | Loop | Invoke | SuperInvoke => 2,
                ConstantLong
                | GetGlobalLong
                | SetGlobalLong
                | DefineGlobalLong
                | DefineGlobalConstLong
                | GetLocalLong
                | SetLocalLong => 3,
                Closure => 1 + self.upvalue_code_len(offset),
            }
    }

    fn upvalue_code_len(&self, closure_offset: usize) -> usize {
        let code = self.chunk.code();
        let constant = code[closure_offset + 1];
        let value = self.chunk.get_constant(constant);
        value.as_function().upvalue_count * 2
    }

    fn debug_constant_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
    ) -> std::fmt::Result {
        let constant_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 1]);
        write!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {:>OPERAND_ALIGNMENT$}",
            name,
            *constant_index,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;
        writeln!(
            f,
            " '{}'",
            *self.chunk.get_constant(*constant_index.as_ref())
        )
    }

    fn debug_constant_long_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
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
            *self.chunk.get_constant(*constant_index.as_ref()),
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
    ) -> std::fmt::Result {
        writeln!(f, "{name}")
    }

    fn debug_byte_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
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
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let jump = (usize::from(code[offset.as_ref() + 1]) << 8)
            + (usize::from(code[offset.as_ref() + 2]));
        let target = *offset + self.instruction_len(*offset);
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
    ) -> std::fmt::Result {
        let mut offset = *offset + 1;

        let code = self.chunk.code();
        //eprintln!("{:?}", &code[offset..]);
        let constant = code[offset];
        offset += 1;

        let value = self.chunk.get_constant(constant);
        writeln!(
            f,
            "{name:-OPCODE_NAME_ALIGNMENT$} {constant:>OPERAND_ALIGNMENT$} {value}",
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )?;

        let function = value.as_function();
        //eprintln!("{} {}", *function.name, function.upvalue_count);
        for _ in 0..function.upvalue_count {
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
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant = code[offset.as_ref() + 1];
        let arg_count = code[offset.as_ref() + 2];
        let constant_value = self.chunk.get_constant(constant);
        let formatted_name = format!("{name} ({arg_count} args)");
        writeln!(
            f,
            "{formatted_name:-OPCODE_NAME_ALIGNMENT$} {constant:>OPERAND_ALIGNMENT$} '{constant_value}'",
            OPCODE_NAME_ALIGNMENT =self.opcode_name_alignment, OPERAND_ALIGNMENT = self.operand_alignment
        )
    }
}

macro_rules! disassemble {
    (
        $self:ident,
        $f:ident,
        $offset:ident,
        $m:expr,
        $(
            $k:ident(
                $($v:ident),* $(,)?
            )
        ),* $(,)?
    ) => {paste! {
        match $m {
            $($(
                OpCode::$v => $self.[<debug_ $k _opcode>]($f, stringify!([<OP_ $v:snake:upper>]), $offset)
            ),*),*
        }
    }}
}

impl std::fmt::Debug for InstructionDisassembler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self.chunk.code();
        let offset = self.offset;

        write!(f, "{:04} ", *offset.as_ref())?;
        if *offset.as_ref() > 0
            && self.chunk.get_line(offset) == self.chunk.get_line(CodeOffset(offset.as_ref() - 1))
        {
            write!(f, "   | ")?;
        } else {
            write!(
                f,
                "{:>OPERAND_ALIGNMENT$} ",
                *self.chunk.get_line(offset),
                OPERAND_ALIGNMENT = self.operand_alignment
            )?;
        }

        let opcode = OpCode::try_from_primitive(code[*offset.as_ref()])
            .unwrap_or_else(|_| panic!("Unknown opcode: {}", code[*offset.as_ref()]));

        disassemble!(
            self,
            f,
            offset,
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
            closure(Closure),
            byte(
                Call, GetUpvalue, SetUpvalue, Class, GetLocal, SetLocal, BuildList, BuildSet,
                BuildDict, DupN, ImportFrom, ImportAs,
            ),
            byte_long(GetLocalLong, SetLocalLong),
            jump(Jump, JumpIfFalse, JumpIfTrue, Loop),
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
                Negate,
                Nil,
                StopIteration,
                Not,
                Pop,
                Return,
                Subtract,
                True,
                Import,
                LoadOne,
                LoadTwo,
                LoadZero,
                LoadMinusOne,
                LoadOnef,
                LoadZerof,
                Swap,
            ),
        )?;
        Ok(())
    }
}

#[cfg(test)]
#[test]
fn opcode_size() {
    assert_eq!(std::mem::size_of::<OpCode>(), 1);
}

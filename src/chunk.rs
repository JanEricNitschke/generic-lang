use derivative::Derivative;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use paste::paste;
use shrinkwraprs::Shrinkwrap;

use crate::{
    heap::{StringId, ValueId},
    types::Line,
};

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

#[derive(IntoPrimitive, TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    ConstantLong,
    Closure,

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

    Equal,
    Greater,
    Less,

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

    Return,

    Class,
    SetProperty,
    GetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,

    BuildList,
    IndexSubscript,
    StoreSubscript,
}

impl OpCode {
    pub const fn to_long(self) -> Self {
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

#[derive(PartialEq, Derivative, Clone)]
#[derivative(PartialOrd)]
pub struct Chunk {
    name: StringId,
    pub code: Vec<u8>,
    #[derivative(PartialOrd = "ignore")]
    lines: Vec<(usize, Line)>,
    constants: Vec<ValueId>,
}

impl Chunk {
    pub fn new(name: StringId) -> Self {
        Self {
            name,
            code: Vec::default(),
            lines: Vec::default(),
            constants: Vec::default(),
        }
    }

    pub fn constants(&self) -> &[ValueId] {
        &self.constants
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_constant<T>(&self, index: T) -> &ValueId
    where
        T: Into<usize>,
    {
        &self.constants[index.into()]
    }

    pub fn write<T>(&mut self, what: T, line: Line)
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

    pub fn patch<T>(&mut self, offset: CodeOffset, what: T)
    where
        T: Into<u8>,
    {
        self.code[*offset] = what.into();
    }

    pub fn make_constant(&mut self, what: ValueId) -> ConstantLongIndex {
        self.constants.push(what);
        ConstantLongIndex(self.constants.len() - 1)
    }

    pub fn write_constant(&mut self, what: ValueId, line: Line) -> bool {
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

    pub fn write_24bit_number(&mut self, what: usize, line: Line) -> bool {
        let (a, b, c, d) = crate::bitwise::get_4_bytes(what);
        if a > 0 {
            return false;
        }
        self.write(b, line);
        self.write(c, line);
        self.write(d, line);
        true
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

// Debug helpers
pub struct InstructionDisassembler<'chunk> {
    chunk: &'chunk Chunk,
    pub offset: CodeOffset,
}

impl<'chunk> InstructionDisassembler<'chunk> {
    #[must_use]
    pub const fn new(chunk: &'chunk Chunk) -> Self {
        Self {
            chunk,
            offset: CodeOffset(0),
        }
    }

    #[allow(clippy::enum_glob_use)]
    pub fn instruction_len(&self, offset: usize) -> usize {
        use OpCode::*;
        let opcode = OpCode::try_from_primitive(self.chunk.code[offset]).unwrap();
        std::mem::size_of::<OpCode>()
            + match opcode {
                Negate | Add | Subtract | Multiply | Divide | Mod | Exp | FloorDiv | BitAnd
                | BitOr | BitXor | Nil | True | False | Not | Equal | Greater | Less | Pop
                | Dup | CloseUpvalue | Inherit | IndexSubscript | StoreSubscript => 0,
                Constant | GetLocal | SetLocal | GetGlobal | SetGlobal | DefineGlobal
                | DefineGlobalConst | Call | Return | GetUpvalue | SetUpvalue | Class
                | GetProperty | SetProperty | Method | GetSuper | BuildList | DupN => 1,
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
        let value = &**self.chunk.get_constant(constant);
        value.as_function().upvalue_count * 2
    }

    fn debug_constant_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
    ) -> std::fmt::Result {
        let constant_index = ConstantIndex(self.chunk.code()[offset.as_ref() + 1]);
        write!(f, "{:-16} {:>4}", name, *constant_index,)?;
        writeln!(
            f,
            " '{}'",
            **self.chunk.get_constant(*constant_index.as_ref())
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
            "{:-16} {:>4} '{}'",
            name,
            *constant_index,
            **self.chunk.get_constant(*constant_index.as_ref())
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
        writeln!(f, "{name:-16} {slot:>4}")
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
        writeln!(f, "{name:-16} {slot:>4}")
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
        writeln!(f, "{:-16} {:>4} -> {}", name, *offset, target)
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

        let value = &**self.chunk.get_constant(constant);
        writeln!(f, "{name:-16} {constant:>4} {value}")?;

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
                "{:04}    |                     {} {}",
                offset - 2,
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
        let constant_value = &**self.chunk.get_constant(constant);
        writeln!(
            f,
            "{name:-16} ({arg_count} args) {constant:4} {constant_value}"
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

impl<'chunk> std::fmt::Debug for InstructionDisassembler<'chunk> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self.chunk.code();
        let offset = self.offset;

        write!(f, "{:04} ", *offset.as_ref())?;
        if *offset.as_ref() > 0
            && self.chunk.get_line(offset) == self.chunk.get_line(CodeOffset(offset.as_ref() - 1))
        {
            write!(f, "   | ")?;
        } else {
            write!(f, "{:>4} ", *self.chunk.get_line(offset))?;
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
            byte(Call, GetUpvalue, SetUpvalue, Class, GetLocal, SetLocal, BuildList, DupN),
            byte_long(GetLocalLong, SetLocalLong),
            jump(Jump, JumpIfFalse, JumpIfTrue, Loop),
            invoke(Invoke, SuperInvoke),
            simple(
                Add,
                CloseUpvalue,
                Divide,
                Dup,
                Equal,
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
                Not,
                Pop,
                Return,
                Subtract,
                True,
                IndexSubscript,
                StoreSubscript,
            ),
        )?;
        Ok(())
    }
}

impl Chunk {
    pub fn get_line(&self, offset: CodeOffset) -> Line {
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

#[cfg(test)]
#[test]
fn opcode_size() {
    assert_eq!(std::mem::size_of::<OpCode>(), 1);
}

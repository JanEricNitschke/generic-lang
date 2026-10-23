use super::{Chunk, CodeOffset, ConstantIndex, ConstantLongIndex, OpCode};
#[cfg(feature = "print_code")]
use crate::config::LAMBDA_NAME;
use crate::{heap::Heap, value::Value};
use convert_case::{Case, Casing};
use num_enum::TryFromPrimitive;
use paste::paste;
use std::fmt::Debug;
use std::mem::size_of;
use strum::IntoEnumIterator;

fn max_name_length() -> usize {
    OpCode::iter()
        .map(|v| v.as_ref().to_case(Case::UpperSnake).len())
        .max()
        .unwrap_or(0)
}

#[cfg(feature = "print_code")]
impl Chunk {
    pub(crate) fn to_string(&self, heap: &Heap) -> String {
        let name = self.name.to_value(heap);

        let mut result = if name == LAMBDA_NAME {
            format!(
                "== {name} ({:?}) ==\n",
                self.locations.first().map(|loc| loc.source.end_line)
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
pub(crate) struct InstructionDisassembler<'chunk, 'heap> {
    chunk: &'chunk Chunk,
    pub(crate) offset: CodeOffset,
    operand_alignment: usize,
    opcode_name_alignment: usize,
    heap: &'heap Heap,
}

impl<'chunk, 'heap> InstructionDisassembler<'chunk, 'heap> {
    #[must_use]
    pub(crate) fn new(chunk: &'chunk Chunk, heap: &'heap Heap) -> Self {
        Self {
            chunk,
            offset: CodeOffset(0),
            operand_alignment: 4,
            // +3 because we add "OP_" to the start.
            opcode_name_alignment: max_name_length() + 3,
            heap,
        }
    }

    #[allow(clippy::enum_glob_use)]
    fn instruction_len(&self, offset: usize, heap: &Heap) -> usize {
        use OpCode::*;
        let opcode = OpCode::try_from_primitive(self.chunk.code[offset]).unwrap();
        size_of::<OpCode>()
            + match opcode {
                Nil | True | False | StopIteration | LoadZero | LoadOne | LoadTwo
                | LoadMinusOne | LoadZerof | LoadOnef | Pop | Dup | Swap | CloseUpvalue
                | Return | ReturnGenerator | Yield | Negate | Not | Add | Subtract | Multiply
                | Divide | FloorDiv | Mod | Exp | BitAnd | BitOr | BitXor | Is | Equal
                | NotEqual | Less | LessEqual | Greater | GreaterEqual | Inherit
                | BuildRational | BuildRangeExclusive | BuildRangeInclusive
                | BuildInterpolation | BuildTemplate | PopHandler | CompareException | Throw
                | Reraise => 0,
                Constant | DupN | GetLocal | SetLocal | GetGlobal | SetGlobal | DefineGlobal
                | DefineGlobalConst | GetUpvalue | SetUpvalue | Call | Class | Method
                | GetProperty | SetProperty | GetSuper | BuildList | BuildTuple | BuildSet
                | BuildDict | BuildFstring => 1,
                Jump | JumpIfNil | JumpIfFalse | JumpIfTrue | PopJumpIfFalse | PopJumpIfTrue
                | JumpIfFalseOrPop | JumpIfTrueOrPop | Loop | Invoke | SuperInvoke
                | ClassVariable | Import | RegisterCatches => 2,
                ConstantLong
                | GetLocalLong
                | SetLocalLong
                | GetGlobalLong
                | SetGlobalLong
                | DefineGlobalLong
                | DefineGlobalConstLong
                | ImportAs => 3,
                Closure => 1 + self.upvalue_code_len(offset, heap),
                ImportFrom => 2 + self.import_from_len(offset),
                // segment-count byte + a ceil(segments / 8)-byte spread bitmap.
                CallUnpack => 1 + self.spread_bitmap_len(offset + 1),
                // method-name byte + segment count + the spread bitmap.
                InvokeUnpack | SuperInvokeUnpack => 2 + self.spread_bitmap_len(offset + 2),
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

    /// Bytes of spread bitmap for the segment-count byte at `count_offset`:
    /// `ceil(segments / 8)`.
    fn spread_bitmap_len(&self, count_offset: usize) -> usize {
        (self.chunk.code[count_offset] as usize).div_ceil(8)
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

    fn debug_class_variable_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant = code[offset.as_ref() + 1];
        let has_default = code[offset.as_ref() + 2];
        let constant_value = self.chunk.get_constant(constant);
        let formatted_name = format!("{name} (default: {})", has_default != 0);
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

    /// `OP_UNPACK_CALL`: a segment count followed by the spread bitmap.
    fn debug_call_unpack_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        _heap: &Heap,
    ) -> std::fmt::Result {
        let segments = self.chunk.code()[offset.as_ref() + 1];
        let bitmap = self.spread_bitmap_binary(offset.as_ref() + 2, segments);
        writeln!(
            f,
            "{:-OPCODE_NAME_ALIGNMENT$} {segments:>OPERAND_ALIGNMENT$} segments [{bitmap}]",
            name,
            OPCODE_NAME_ALIGNMENT = self.opcode_name_alignment,
            OPERAND_ALIGNMENT = self.operand_alignment
        )
    }

    /// `OP_INVOKE_UNPACK` / `OP_SUPER_INVOKE_UNPACK`: a method-name constant, a
    /// segment count, then the spread bitmap.
    fn debug_invoke_unpack_opcode(
        &self,
        f: &mut std::fmt::Formatter,
        name: &str,
        offset: CodeOffset,
        heap: &Heap,
    ) -> std::fmt::Result {
        let code = self.chunk.code();
        let constant = code[offset.as_ref() + 1];
        let segments = code[offset.as_ref() + 2];
        let constant_value = self.chunk.get_constant(constant);
        let bitmap = self.spread_bitmap_binary(offset.as_ref() + 3, segments);
        let formatted_name = format!("{name} ({segments} segments [{bitmap}])");
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

    /// Render the `ceil(segments / 8)` spread-bitmap bytes at `start` as binary,
    /// least-significant bit first, so segment `i`'s flag reads left to right.
    fn spread_bitmap_binary(&self, start: usize, segments: u8) -> String {
        let code = self.chunk.code();
        (0..(segments as usize).div_ceil(8))
            .map(|byte| format!("{:08b}", code[start + byte].reverse_bits()))
            .collect::<Vec<_>>()
            .join(" ")
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
                *self.chunk.get_location(offset).source.end_line,
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
                JumpIfNil,
                Loop,
                RegisterCatches
            ),
            invoke(Invoke, SuperInvoke),
            class_variable(ClassVariable),
            simple(
                Add,
                CloseUpvalue,
                Divide,
                Dup,
                Equal,
                GreaterEqual,
                LessEqual,
                NotEqual,
                Is,
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
                BuildInterpolation,
                BuildTemplate,
                Negate,
                Nil,
                StopIteration,
                Not,
                Pop,
                Return,
                ReturnGenerator,
                Yield,
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
            call_unpack(CallUnpack),
            invoke_unpack(InvokeUnpack, SuperInvokeUnpack),
        )?;
        Ok(())
    }
}

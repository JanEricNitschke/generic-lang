//! Backend of the compiler. This module is responsible for emitting bytecode.

use crate::{
    chunk::{CodeOffset, OpCode},
    scanner::{Token, TokenKind},
    types::{Column, Line, NumberEncoding, ReturnMode},
    value::{GenericInt, Number, Value},
};

use super::{Compiler, FunctionType};

impl<'scanner> Compiler<'scanner, '_> {
    pub(super) fn emit_byte<T>(&mut self, byte: T, line: Line)
    where
        T: Into<u8>,
    {
        self.current_chunk_mut().write(byte, line);
    }

    pub(super) fn emit_byte_with_location<T>(&mut self, byte: T, line: Line, column: Column)
    where
        T: Into<u8>,
    {
        self.current_chunk_mut()
            .write_with_location(byte, line, column);
    }

    pub(super) fn emit_byte_at_current_location<T>(&mut self, byte: T)
    where
        T: Into<u8>,
    {
        let line = self.line();
        let column = self.column();
        self.emit_byte_with_location(byte, line, column);
    }

    pub(super) fn emit_24bit_number(&mut self, number: usize) -> bool {
        let line = self.line();
        let column = self.column();
        self.current_chunk_mut()
            .write_24bit_number_with_location(number, line, column)
    }

    pub(super) fn emit_bytes<T1, T2>(&mut self, byte1: T1, byte2: T2, line: Line)
    where
        T1: Into<u8>,
        T2: Into<u8>,
    {
        self.current_chunk_mut().write(byte1, line);
        self.current_chunk_mut().write(byte2, line);
    }

    pub(super) fn emit_return(&mut self) {
        let line = self.line();
        if self.function_type() == FunctionType::Initializer {
            self.emit_bytes(OpCode::GetLocal, 0, line);
        } else {
            self.emit_byte(OpCode::Nil, line);
        }
        self.emit_byte(OpCode::Return, line);
    }

    pub(super) fn end(&mut self, return_mode: ReturnMode) {
        match return_mode {
            ReturnMode::Raw => self.emit_byte(OpCode::Return, self.line()),
            ReturnMode::Normal => self.emit_return(),
        }

        #[cfg(feature = "print_code")]
        self.print_code_info();
    }

    pub(super) fn emit_constant<T>(&mut self, value: T)
    where
        T: Into<Value>,
    {
        let line = self.line();
        let column = self.column();
        let value = value.into();
        match value {
            Value::Number(Number::Integer(GenericInt::Small(1))) => {
                self.emit_byte_with_location(OpCode::LoadOne, line, column);
            }
            Value::Number(Number::Integer(GenericInt::Small(2))) => {
                self.emit_byte_with_location(OpCode::LoadTwo, line, column);
            }
            Value::Number(Number::Integer(GenericInt::Small(-1))) => {
                self.emit_byte_with_location(OpCode::LoadMinusOne, line, column);
            }
            Value::Number(Number::Integer(GenericInt::Small(0))) => {
                self.emit_byte_with_location(OpCode::LoadZero, line, column);
            }
            Value::Number(Number::Float(0.0)) => {
                self.emit_byte_with_location(OpCode::LoadZerof, line, column);
            }
            Value::Number(Number::Float(1.0)) => {
                self.emit_byte_with_location(OpCode::LoadOnef, line, column);
            }
            _ => {
                if !self
                    .current_chunk_mut()
                    .write_constant_with_location(value, line, column)
                {
                    self.error("Too many constants in one chunk.");
                }
            }
        }
    }

    pub(super) fn emit_jump(&mut self, instruction: OpCode) -> CodeOffset {
        let line = self.line();
        self.emit_byte(instruction, line);
        let retval = CodeOffset(self.current_chunk().code().len() - 1);
        self.emit_byte(0xff, line);
        self.emit_byte(0xff, line);
        retval
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn patch_jump(&mut self, jump_offset: CodeOffset) {
        let jump_length = self.current_chunk().code().len() - *jump_offset - 3; // 3: length of the jump instruction + its arg

        if jump_length > usize::from(u16::MAX) {
            self.error("Too much code to jump over.");
        }

        self.current_chunk_mut()
            .patch(CodeOffset(*jump_offset + 1), (jump_length >> 8) as u8);
        self.current_chunk_mut()
            .patch(CodeOffset(*jump_offset + 2), jump_length as u8);
    }

    pub(super) fn patch_break_jumps(&mut self) {
        while let Some(break_jump) = self
            .last_loop_state_mut()
            .as_mut()
            .unwrap()
            .break_jumps
            .pop()
        {
            self.patch_jump(break_jump);
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn emit_loop(&mut self, loop_start: CodeOffset) {
        let offset = self.current_chunk().code().len() - *loop_start + 3; // 3: length of the loop instruction + its arg
        let line = self.line();

        self.emit_byte(OpCode::Loop, line);
        if offset > usize::from(u16::MAX) {
            self.error("Loop body too large.");
        }

        self.emit_byte((offset >> 8) as u8, line);
        self.emit_byte(offset as u8, line);
    }

    pub(super) fn emit_number(&mut self, n: usize, encoding: NumberEncoding) -> bool {
        match encoding {
            NumberEncoding::Long => self.emit_24bit_number(n),
            NumberEncoding::Short => {
                if let Ok(n) = u8::try_from(n) {
                    self.emit_byte_at_current_location(n);
                    true
                } else {
                    false
                }
            }
        }
    }

    pub(super) fn synthetic_token(&self, kind: TokenKind) -> Token<'scanner> {
        Token {
            kind,
            lexeme: match kind {
                TokenKind::Super => "super",
                TokenKind::This => "this",
                _ => unimplemented!(),
            },
            line: self.line(),
            column: Column(1), // Synthetic tokens default to column 1
        }
    }

    pub(super) fn synthetic_identifier_token(&self, identifier: &'scanner str) -> Token<'scanner> {
        Token {
            kind: TokenKind::Identifier,
            lexeme: identifier,
            line: self.line(),
            column: Column(1), // Synthetic tokens default to column 1
        }
    }
}

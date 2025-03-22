use super::VM;
use crate::heap::StringId;
use crate::value::Value;

impl VM {
    /// Read the next byte from the current frame's bytecode.
    pub(super) fn read_byte(&mut self) -> u8 {
        let frame = self.callstack.current_mut();
        let index = frame.ip;
        frame.ip += 1;
        self.callstack.code_byte(index, &self.heap)
    }

    /// Read a 16 bit number from the current frame's bytecode.
    ///
    /// Reads two bytes and builds a 16 bit number from them.
    pub(super) fn read_16bit_number(&mut self) -> usize {
        (usize::from(self.read_byte()) << 8) + (usize::from(self.read_byte()))
    }

    /// Read a 24 bit number from the current frame's bytecode.
    ///
    /// Reads three bytes and builds a 24 bit number from them.
    pub(super) fn read_24bit_number(&mut self) -> usize {
        (usize::from(self.read_byte()) << 16)
            + (usize::from(self.read_byte()) << 8)
            + (usize::from(self.read_byte()))
    }

    /// Read a constant index from the current frame's bytecode.
    ///
    /// If `long` is true then a 24 bit number is read, otherwise a single byte.
    pub(super) fn read_constant_index(&mut self, long: bool) -> usize {
        if long {
            self.read_24bit_number()
        } else {
            usize::from(self.read_byte())
        }
    }

    pub(super) fn read_constant_value(&self, index: usize) -> Value {
        *self
            .callstack
            .function()
            .to_value(&self.heap)
            .chunk
            .get_constant(index)
    }

    /// Read the value of the constant specified by the next byte.
    ///
    /// First reads the index of the constant from the bytecode.
    /// Then grabs the value corresponding to that index from the current function's constants.
    pub(super) fn read_constant(&mut self, long: bool) -> Value {
        let index = self.read_constant_index(long);
        self.read_constant_value(index)
    }

    /// Read the value of the string constant specified by the next byte.
    pub(super) fn read_string(&mut self, opcode_name: &str) -> StringId {
        let constant = self.read_constant(false);
        match &constant {
            Value::String(string_id) => *string_id,
            x => {
                panic!(
                    "Non-string method name to {opcode_name}: `{}`",
                    x.to_string(&self.heap)
                );
            }
        }
    }
}

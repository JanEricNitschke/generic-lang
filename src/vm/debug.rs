#[cfg(feature = "trace_execution")]
use crate::chunk::InstructionDisassembler;
#[cfg(feature = "trace_execution")]
use crate::vm::VM;

#[cfg(feature = "trace_execution")]
impl VM {
    pub fn trace_execution(&self) {
        // lightweight skip check
        if cfg!(feature = "trace_execution_builtin")
            || !self
                .modules
                .last()
                .unwrap()
                .to_value(&self.heap)
                .path
                .iter()
                .any(|comp| comp == "builtins")
        {
            let function = &self.callstack.function();
            let mut disassembler =
                InstructionDisassembler::new(&function.to_value(&self.heap).chunk, &self.heap);
            *disassembler.offset = self.callstack.current().ip;

            #[cfg(feature = "trace_execution_verbose")]
            self.print_verbose_state();

            println!(
                "          [ {} ]",
                self.stack
                    .iter()
                    .map(|v| v
                        .to_string(&self.heap)
                        .replace('\n', " ")
                        .chars()
                        .take(20)
                        .collect::<String>())
                    .collect::<Vec<_>>()
                    .join(" ][ ")
            );
            print!("{disassembler:?}");
            #[cfg(feature = "trace_execution_verbose")]
            println!("----------------------");
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    pub fn print_verbose_state(&self) {
        self.print_module_details();
        self.print_callstack_details();
        self.print_globals();
        self.print_exception_handlers();
        self.print_open_upvalues();
        self.print_exception_state();
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_module_details(&self) {
        println!(
            "Current module: {} at module depth {} and Function: {} at total call depth {}.",
            self.modules
                .last()
                .expect("Module underflow in disassembler")
                .to_value(&self.heap)
                .name
                .to_value(&self.heap),
            self.modules.len(),
            self.callstack
                .function()
                .to_value(&self.heap)
                .name
                .to_value(&self.heap),
            self.callstack.len()
        );

        println!("Module stack details:");

        let max_name_width = self
            .modules
            .iter()
            .map(|module| module.to_value(&self.heap).name.to_value(&self.heap).len())
            .max()
            .unwrap_or(0);
        for (i, module) in self.modules.iter().enumerate() {
            let name = module.to_value(&self.heap).name.to_value(&self.heap);
            println!("  [{i:3}] {name:max_name_width$}",);
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_callstack_details(&self) {
        println!("Call stack details:");

        let max_name_width = self
            .callstack
            .iter()
            .map(|frame| {
                let closure = frame.closure(&self.heap);
                let function = closure.function.to_value(&self.heap);
                function.name.to_value(&self.heap).len()
            })
            .max()
            .unwrap_or(0);
        for (i, frame) in self.callstack.iter().enumerate() {
            let closure = frame.closure(&self.heap);
            let function = closure.function.to_value(&self.heap);
            println!(
                "  [{:3}] {:width_name$} (arity: {:3}, upvalue_count: {:3}, ip: {:5})",
                i,
                function.name.to_value(&self.heap),
                function.arity,
                function.upvalue_count,
                frame.ip,
                width_name = max_name_width,
            );
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_globals(&self) {
        let module_globals = &self.defining_module().to_value(&self.heap).globals;
        if module_globals.is_empty() {
            println!("No globals defined in current module");
            return;
        }

        let mut user_globals: Vec<_> = module_globals.iter().collect();
        user_globals.sort_by_key(|(name, _)| name.to_value(&self.heap));

        println!("Globals ({} total):", user_globals.len());

        let max_name_width = user_globals
            .iter()
            .map(|(name, _)| name.to_value(&self.heap).len())
            .max()
            .unwrap_or(0);
        let max_value_width = user_globals
            .iter()
            .map(|(_, global)| global.value.to_string(&self.heap).len())
            .max()
            .unwrap_or(0);

        for (name, global) in user_globals {
            println!(
                "  '{:width_name$}': {:width_value$} ({})",
                name.to_value(&self.heap),
                global.value.to_string(&self.heap),
                if global.is_mutable() {
                    "mutable"
                } else {
                    "const"
                },
                width_name = max_name_width,
                width_value = max_value_width
            );
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_exception_handlers(&self) {
        if self.exception_handlers.is_empty() {
            println!("No active exception handlers");
            return;
        }

        println!(
            "Active exception handlers ({} total):",
            self.exception_handlers.len()
        );

        for (i, handler) in self.exception_handlers.iter().enumerate() {
            println!(
                "  [{:3}] modules_to_keep: {:3}, frames_to_keep: {:3}, ip: {:5}, stack_length: {:5}",
                i,
                handler.modules_to_keep,
                handler.frames_to_keep,
                handler.ip,
                handler.stack_length,
            );
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_open_upvalues(&self) {
        if self.open_upvalues.is_empty() {
            println!("No open upvalues");
            return;
        }

        println!("Open upvalues ({} total):", self.open_upvalues.len());

        for (i, upvalue_id) in self.open_upvalues.iter().enumerate() {
            let upvalue = upvalue_id.to_value(&self.heap);
            let detail = match upvalue {
                crate::value::Upvalue::Open(stack_index) => {
                    format!("Open at stack index: {stack_index}")
                }
                crate::value::Upvalue::Closed(value) => {
                    format!("Closed with value: {}", value.to_string(&self.heap))
                }
            };

            println!("  [{i:3}] {detail:25}",);
        }
    }

    #[cfg(feature = "trace_execution_verbose")]
    fn print_exception_state(&self) {
        println!(
            "Handling exception: {}, encountered hard exception: {}",
            self.handling_exception, self.encountered_hard_exception
        );
    }
}

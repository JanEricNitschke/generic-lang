use crate::config::SPREAD_BITMAP_BYTES;
use crate::heap::ClassId;
use crate::value::NativeClass;
#[cfg(feature = "plugins")]
use crate::value::{ClassKind, PluginInstance};
use crate::vm::ExceptionKind::{
    AttributeError, ConstReassignmentError, ImportError, RecursionError, TypeError, ValueError,
};
use crate::vm::arithmetics::IntoResultValue;
use crate::vm::errors::{Return, VmErrorKind, VmResult};
use crate::vm::exception_handling::RegionSnapshot;
#[cfg(feature = "plugins")]
use crate::vm::plugins::trampolines::call_plugin_method;
use crate::{
    chunk::OpCode,
    heap::{NativeFunctionId, NativeMethodId, StringId, UpvalueId},
    types::{Comparison, EqualityMode, JumpCondition, NumberEncoding, RangeType},
    value::{Class, Closure, Instance, Number, Upvalue, Value, get_native_class_id},
};

use tinyvec::TinyVec;

use super::{Global, VM};

/// Native-call arguments are copied off the VM stack into an inline-capacity
/// vector: stack storage for up to this many arguments, heap beyond (only
/// reachable through the variadic constructors).
const INLINE_NATIVE_ARGS: usize = 8;

// Execute a function (rust side)
impl VM {
    /// Execute and immediately run a function.
    ///
    /// This is used when (runtime) class specific information is needed
    /// in native functions like `print` or `str`.
    ///
    /// Pushes the closure onto the stack and callstack. Then directly
    /// executes all of the bytecode for it before returning to the main loop.
    ///
    /// # Exception contract
    ///
    /// The receiver and arguments are consumed either way, and exactly one
    /// value is left on the stack top:
    /// - `Ok(..)`: the callee's result.
    /// - `Err(Exception)`: the pending exception that escaped the callee
    ///   (its leftover frames and stack are cleaned up here). The caller may
    ///   pop the exception and handle it, or propagate with `?` - the
    ///   dispatch loops resolve an escaping pending exception against the
    ///   surrounding handlers.
    pub(crate) fn invoke_and_run_function(
        &mut self,
        method_name: StringId,
        arg_count: u8,
    ) -> VmResult {
        // The region starts below the callee and its arguments.
        let entry_stack = self.stack.len() - usize::from(arg_count) - 1;
        self.run_reentrant_call(entry_stack, |vm| vm.invoke(method_name, arg_count))
    }

    /// Call an arbitrary callable already on the stack with its `arg_count`
    /// arguments above it, running its bytecode to completion. The
    /// value-addressed twin of [`Self::invoke_and_run_function`] (which looks
    /// the callee up by method name); same exception contract.
    pub(crate) fn call_value_and_run(&mut self, arg_count: u8) -> VmResult {
        let entry_stack = self.stack.len() - usize::from(arg_count) - 1;
        let callee = self.stack[entry_stack];
        self.run_reentrant_call(entry_stack, |vm| vm.call_value(callee, arg_count))
    }

    /// Shared engine behind [`Self::invoke_and_run_function`] and
    /// [`Self::call_value_and_run`]. `entry_stack` is the callee's slot (its
    /// arguments sit above it); `dispatch` performs the actual call and reports
    /// through the callstack whether it pushed a bytecode frame. Runs that frame
    /// to completion and enforces the shared exception contract.
    fn run_reentrant_call<F>(&mut self, entry_stack: usize, dispatch: F) -> VmResult
    where
        F: FnOnce(&mut Self) -> VmResult,
    {
        let entry = RegionSnapshot {
            stack: entry_stack,
            ..self.current_region()
        };

        // Bound the host stack: native dunder recursion (`__str__` on nested
        // containers, `__eq__`/`__lt__`, `__hash__`) re-enters here without
        // pushing a bytecode frame, so `FRAMES_MAX` cannot see it. The counter
        // is decremented unconditionally below (no `?` between), and the
        // over-limit throw is cleaned up by the same `Err` arm as a real
        // escaping exception, so the pushed receiver and arguments are unwound.
        self.reentry_depth += 1;
        let result = if self.reentry_depth > crate::config::REENTRY_MAX {
            self.throw(RecursionError, "maximum recursion depth exceeded")
        } else {
            // Whether the callee needs its bytecode run cannot be decided from
            // the dispatch itself: it may bind to an instance *field* instead
            // (e.g. a native function stored under `__str__`, or a field-held
            // closure), so trust the callstack - run exactly when a frame was
            // pushed. Anything that completed natively already left its result
            // on the stack.
            let frames_before = self.callstack.len();
            match dispatch(self) {
                Ok(_) if self.callstack.len() > frames_before => self.run_function(),
                Ok(_) => Ok(None),
                err => err,
            }
        };
        self.reentry_depth -= 1;

        match result {
            Err(VmErrorKind::Exception(kind)) => {
                // A pending exception escaped the callee. The run loop hands
                // escapes over untruncated (see `run_function_from_depth`),
                // so this is where the region is cleaned: drop the callee's
                // leftover frames and stack, keep the exception on top in
                // place of the result.
                let exception = self.stack.pop().expect("Pending exception missing");
                self.unwind_region(entry);
                self.stack.push(exception);
                Err(VmErrorKind::Exception(kind))
            }
            other => other,
        }
    }

    /// Run the closure currently on top of the callstack.
    pub(crate) fn run_function(&mut self) -> VmResult {
        self.run_function_from_depth(self.callstack.len())
    }

    /// Run the bytecode of the callstack region `callstack[call_depth-1..]`
    /// until it exits: a return/yield past the region base, or an escaping
    /// error.
    ///
    /// A pending exception is resolved against the innermost handler if that
    /// handler lies within this region - execution then continues at the
    /// catch block. Otherwise the pending exception escapes to whoever
    /// entered the region, which may handle or propagate it. Hard runtime
    /// errors are fatal and always propagate.
    ///
    /// # Escapes leave the region dirty - every caller cleans up itself
    ///
    /// On an escape NOTHING is truncated here: the region's frames stay on
    /// the callstack and its values on the stack (below the pending
    /// exception), so everything remains GC-rooted until the caller decides.
    /// The callers and their cleanup:
    /// - [`Self::invoke_and_run_function`] (every dunder/native re-entry):
    ///   restores its pre-call snapshot in its `Err` arm, so natives always
    ///   see "state exactly as before the call, exception on top".
    /// - `Generator::resume_with` (send/next/raise): performs no cleanup.
    ///   The dead generator frame and its spliced values stay on the VM;
    ///   that is sound because the generator is marked `Completed` (and
    ///   thrown away) and the generator natives return immediately, so the
    ///   dirty state is only ever truncated by the caller's dispatch loop
    ///   when it resolves the pending exception against an outer handler -
    ///   or the program aborts via the uncaught report.
    ///
    /// # Precondition
    ///
    /// The callstack must hold at least `call_depth` frames - entering with
    /// the region already exited would execute one instruction of the outer
    /// frame before the positional check below fires.
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub(crate) fn run_function_from_depth(&mut self, call_depth: usize) -> VmResult {
        debug_assert!(
            self.callstack.len() >= call_depth,
            "run_function_from_depth entered with its region already exited"
        );
        loop {
            let result = run_instruction!(self);
            match result {
                Err(VmErrorKind::Exception(_)) => {
                    if !self.resolve_pending_exception(call_depth) {
                        return result;
                    }
                }
                Err(VmErrorKind::Runtime(_)) => return result,
                Ok(_) if self.callstack.len() < call_depth => return result,
                Ok(_) => {}
            }
        }
    }
}

// Handle a call (generic side)
impl VM {
    pub(super) fn call(&mut self) -> VmResult {
        let arg_count = self.read_byte();
        let callee = self.stack[self.stack.len() - 1 - usize::from(arg_count)];
        self.call_value(callee, arg_count)
    }

    /// Invoke a value retrieved from an instance or module.
    ///
    /// If it is an instance and the attribute is not a property of the instance
    /// then a method is looked up in the class.
    pub(crate) fn invoke(&mut self, method_name: StringId, arg_count: u8) -> VmResult {
        let receiver = *self
            .peek(arg_count.into())
            .expect("Stack underflow in OP_INVOKE");
        match receiver {
            Value::Instance(instance) => {
                // Callable attribute of the instance
                if let Some(value) = instance
                    .to_value(&self.heap)
                    .fields
                    .get(method_name.to_value(&self.heap))
                {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = *value;
                    self.call_value(*value, arg_count)
                }
                // Method of the class.
                else if instance
                    .to_value(&self.heap)
                    .class
                    .to_value(&self.heap)
                    .methods
                    .contains_key(&method_name)
                {
                    self.invoke_from_class(
                        instance.to_value(&self.heap).class,
                        method_name,
                        arg_count,
                    )
                }
                // Callable class variable, read through the instance.
                else if let Some(value) = instance
                    .to_value(&self.heap)
                    .class
                    .to_value(&self.heap)
                    .class_variable_value(method_name)
                {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = value;
                    self.call_value(value, arg_count)
                }
                // Nothing found.
                else {
                    let message =
                        format!("Undefined property '{}'.", self.heap.strings[method_name]);
                    self.throw(AttributeError, &message)
                }
            }
            // Calling through the class object resolves class variables
            // only, mirroring class property access.
            Value::Class(class) => {
                if let Some(value) = class.to_value(&self.heap).class_variable_value(method_name) {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = value;
                    self.call_value(value, arg_count)
                } else {
                    let message =
                        format!("Undefined property '{}'.", self.heap.strings[method_name]);
                    self.throw(AttributeError, &message)
                }
            }
            Value::Module(module) => {
                if let Some(value) = module.to_value(&self.heap).globals.get(&method_name) {
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = value.value;
                    self.call_value(value.value, arg_count)
                } else {
                    let message = format!(
                        "Function '{}' not defined in module {}.",
                        method_name.to_value(&self.heap),
                        module.to_value(&self.heap).name.to_value(&self.heap)
                    );
                    self.throw(ValueError, &message)
                }
            }
            _ => {
                if let Some(proxy_class) = self.get_proxy_class(receiver) {
                    self.invoke_from_class(proxy_class, method_name, arg_count)
                } else {
                    self.throw(TypeError, "Only instances have methods.")
                }
            }
        }
    }

    /// Invoke a method on an instance directly from its class.
    pub(super) fn invoke_from_class(
        &mut self,
        class: ClassId,
        method_name: StringId,
        arg_count: u8,
    ) -> VmResult {
        let Some(method) = class.to_value(&self.heap).methods.get(&method_name) else {
            let message = format!("Undefined property '{}'.", self.heap.strings[method_name]);
            return self.throw(AttributeError, &message);
        };
        match method {
            Value::Closure(_) => self.execute_call(*method, arg_count),
            Value::NativeMethod(native) => {
                let receiver = *self.peek(arg_count as usize).unwrap();
                self.execute_native_method_call(*native, &receiver, arg_count)
            }
            x => unreachable!(
                "Can only invoke closure or native methods. Got `{}` instead.",
                x.to_string(&self.heap)
            ),
        }
    }

    /// Call the passed value with the passed number of arguments.
    ///
    /// The arguments should reside on top of the stack with the first (leftmost) argument
    /// being the deepest on the stack directly ontop of where the `callee` was taken from.
    ///
    /// Callable values are:
    /// - Closures:
    ///    - Are scheduled directly to be executed.
    /// - Native functions:
    ///   - Are executed directly.
    /// - Classes:
    ///     - Are instantiated and the initializer is called.
    /// - Bound methods:
    ///    - If the bound method is a standard one, it is scheduled for execution.
    ///    - If the bound method is a native one, it is executed directly.
    pub(super) fn call_value(&mut self, callee: Value, arg_count: u8) -> VmResult {
        let call_id = self.heap.string_id(&"__call__");
        match callee {
            Value::Instance(instance)
                if instance
                    .to_value(&self.heap)
                    .has_field_or_method(call_id, &self.heap) =>
            {
                self.invoke(call_id, arg_count)
            }
            Value::Closure(_) => self.execute_call(callee, arg_count),
            Value::NativeFunction(f) => self.execute_native_function_call(f, arg_count),
            Value::Class(class) => {
                let class_data = class.to_value(&self.heap);
                let maybe_initializer = class_data
                    .methods
                    .get(&self.heap.builtin_constants().init_string)
                    .copied();

                let backing = class_data.get_native_superclass(&self.heap, class).map(
                    |native_superclass_id| {
                        let native_superclass = native_superclass_id.to_value(&self.heap);
                        // Plugin class: a Plugin backing with a null opaque ptr;
                        // `__init__` installs the real pointer via set_opaque.
                        #[cfg(feature = "plugins")]
                        if matches!(native_superclass.kind, ClassKind::Plugin(_)) {
                            return NativeClass::Plugin(PluginInstance::empty());
                        }
                        NativeClass::new(native_superclass.name.to_value(&self.heap))
                    },
                );

                let instance_id = self.heap.add_instance(Instance::new(class, backing));
                let stack_index = self.stack.len() - usize::from(arg_count) - 1;
                self.stack[stack_index] = instance_id;
                if let Some(initializer) = maybe_initializer {
                    match initializer {
                        Value::NativeMethod(native_method_id) => self.execute_native_method_call(
                            native_method_id,
                            &instance_id,
                            arg_count,
                        ),
                        _ => self.execute_call(initializer, arg_count),
                    }
                } else if arg_count != 0 {
                    let message = format!("Expected 0 arguments but got {arg_count}.");
                    self.throw(TypeError, &message)
                } else {
                    Ok(None)
                }
            }
            Value::BoundMethod(bound_method) => match bound_method.to_value(&self.heap).method {
                Value::Closure(_) => {
                    let bound_method = bound_method.to_value(&self.heap);
                    let new_stack_base = self.stack.len() - usize::from(arg_count) - 1;
                    self.stack[new_stack_base] = bound_method.receiver;
                    self.execute_call(bound_method.method, arg_count)
                }
                Value::NativeMethod(native_method) => {
                    let receiver = bound_method.to_value(&self.heap).receiver;
                    self.execute_native_method_call(native_method, &receiver, arg_count)
                }
                _ => self.throw(
                    TypeError,
                    "Native methods only bind over closures or native methods.",
                ),
            },
            _ => self.throw(
                TypeError,
                "Can only call functions, classes and instances with a `__call__` method.",
            ),
        }
    }

    /// Execute a normal closure call.
    ///
    /// The arity of the closure is checked against the provided number of arguments.
    /// Then the closure is pushed onto the callstack.
    pub(super) fn execute_call(&mut self, closure_id: Value, arg_count: u8) -> VmResult {
        let closure = closure_id.as_closure();
        let (arity, required, is_variadic) = {
            let function = closure.to_value(&self.heap).function.to_value(&self.heap);
            (
                function.arity,
                function.required_params,
                function.is_variadic,
            )
        };
        let arg_count = usize::from(arg_count);
        // A variadic function has no upper bound; a fixed one caps at `arity`.
        if arg_count < required || (!is_variadic && arg_count > arity) {
            return self.throw(
                TypeError,
                &Self::arity_message(required, arity, is_variadic, arg_count),
            );
        }

        if self.callstack.len() == crate::config::FRAMES_MAX {
            return self.throw(RecursionError, "maximum recursion depth exceeded");
        }

        let stack_base = self.stack.len() - arg_count - 1;
        if is_variadic && arg_count >= arity {
            // Every fixed parameter is already supplied; collect the surplus
            // positional arguments into the `*rest` tuple.
            let overflow = self.stack.split_off(stack_base + 1 + arity);
            let rest = self.new_tuple(overflow);
            self.stack_push(rest);
        } else {
            // Fill the omitted trailing optional parameters from the closure's
            // defaults; the provided arguments already sit in their slots.
            for slot in arg_count..arity {
                let default = closure.to_value(&self.heap).default_values[slot - required];
                self.stack_push(default);
            }
            if is_variadic {
                let rest = self.new_tuple(Vec::new());
                self.stack_push(rest);
            }
        }
        self.callstack
            .push(*closure_id.as_closure(), stack_base, &self.heap);
        Ok(None)
    }

    /// `OP_UNPACK_CALL`: flatten the argument segments and call the callee
    /// sitting below them.
    pub(super) fn call_unpack(&mut self) -> VmResult {
        let arg_count = self.gather_unpacked_arguments()?;
        let callee = *self
            .peek(usize::from(arg_count))
            .expect("Callee missing below unpacked arguments");
        self.call_value(callee, arg_count)
    }

    /// `OP_INVOKE_UNPACK`: flatten the argument segments and invoke `method_name`
    /// on the receiver sitting below them.
    pub(super) fn invoke_unpack(&mut self, method_name: StringId) -> VmResult {
        let arg_count = self.gather_unpacked_arguments()?;
        self.invoke(method_name, arg_count)
    }

    /// `OP_SUPER_INVOKE_UNPACK`: pop the superclass, flatten the argument
    /// segments, and invoke `method_name` from that superclass.
    pub(super) fn super_invoke_unpack(&mut self, method_name: StringId) -> VmResult {
        let superclass = self
            .stack
            .pop()
            .expect("Stack underflow in OP_SUPER_INVOKE_UNPACK");
        let arg_count = self.gather_unpacked_arguments()?;
        self.invoke_from_class(*superclass.as_class(), method_name, arg_count)
    }

    /// Flatten the argument segments of an unpacking call, expanding spreads.
    ///
    /// # Argument unpacking (`f(a, *xs, b)`)
    ///
    /// A call whose argument list contains a spread (`*expr`) cannot know its
    /// final argument count at compile time, so it is compiled differently
    /// from a plain call. Each written argument is a *segment*: the compiler
    /// emits every segment's value as-is (a spread leaves the *iterable*
    /// itself on the stack) and records a spread bitmap - one bit per segment,
    /// set when that segment is a spread. The call is then one of
    /// `OP_UNPACK_CALL` / `OP_INVOKE_UNPACK` / `OP_SUPER_INVOKE_UNPACK` instead
    /// of `OP_CALL` / `OP_INVOKE` / `OP_SUPER_INVOKE`, carrying (after the
    /// method-name operand, for the invoke forms) a segment-count byte and the
    /// `ceil(count / 8)` bitmap bytes.
    ///
    /// ## The spread bitmap and the 255-argument cap
    ///
    /// Segment `i` occupies bit `i % 8` of byte `i / 8`, least-significant bit
    /// first (so segment 0 is bit 0 of byte 0). A call takes at most 255
    /// arguments, so the map never needs more than `ceil(255 / 8) = 32` bytes
    /// ([`SPREAD_BITMAP_BYTES`]); the compiler fills a fixed array of that size
    /// and emits only the `ceil(count / 8)` bytes actually used, and this
    /// routine reads back exactly that many. For a ten-segment call whose
    /// segments 1 and 8 are spreads:
    ///
    /// ```text
    ///   f(a, *b, c, d, e, f, g, h, *i, j)   // segments 1 and 8 are spreads
    ///
    ///   segments 0..=7  -> byte 0 = 0b0000_0010   (bit 1 set: segment 1)
    ///   segment  8..=9  -> byte 1 = 0b0000_0001   (bit 0 set: segment 8)
    /// ```
    ///
    /// The 255 cap is enforced twice. The compiler rejects more than 255
    /// *written* arguments outright. At runtime a spread's length is not known
    /// until it is expanded, so a flattened total above 255 raises a
    /// `TypeError` here - which is also the ceiling on how many arguments a
    /// `*rest` parameter can ever receive.
    ///
    /// This routine reads that count and bitmap, then rewrites the segments on
    /// the stack top into the final positional arguments: a plain segment
    /// contributes its one value, a spread segment is expanded into its
    /// iterable's items (a non-iterable spread raises `TypeError`). The callee
    /// or receiver below the segments is untouched, so afterwards the stack
    /// reads `[callee, arg0, arg1, ...]` and the ordinary call machinery runs.
    /// The count is returned (and checked to fit the 255-argument limit).
    ///
    /// Because expanding a spread re-enters the interpreter (`__iter__` /
    /// `__next__`) and may collect garbage, the growing argument list is built
    /// on the VM stack - never in a `Vec` - so every value stays rooted.
    ///
    /// ```text
    /// var xs = [2, 3];
    /// add(1, *xs, 4);        // add(a, b, c, d)
    ///
    ///   ... GET_GLOBAL add           push the callee
    ///   ... CONSTANT 1               segment 0 (plain)
    ///   ... GET_GLOBAL xs            segment 1 (spread: the list itself)
    ///   ... CONSTANT 4               segment 2 (plain)
    ///   ... UNPACK_CALL  3  [010...] three segments, segment 1 is a spread
    ///
    ///   stack before: [add, 1, [2,3], 4]
    ///   stack after:  [add, 1, 2, 3, 4]     -> call add with 4 arguments
    /// ```
    ///
    /// The definition-side counterpart, `*rest`, is handled in
    /// [`Self::execute_call`]: surplus positional arguments are collected into
    /// a tuple bound to the rest parameter.
    pub(super) fn gather_unpacked_arguments(&mut self) -> VmResult<u8> {
        let n_segments = usize::from(self.read_byte());
        // The `ceil(n / 8)` spread-bitmap bytes follow; segment `i` is a spread
        // when bit `i % 8` of byte `i / 8` is set.
        let mut bitmap = [0u8; SPREAD_BITMAP_BYTES];
        for byte in bitmap.iter_mut().take(n_segments.div_ceil(8)) {
            *byte = self.read_byte();
        }

        // The segments occupy the top of the stack; expand them into arguments
        // pushed above, keeping everything stack-rooted across the re-entrant
        // `collect_items_from_iterable` calls.
        let segment_base = self.stack.len() - n_segments;
        let result_base = self.stack.len();
        for index in 0..n_segments {
            let value = self.stack[segment_base + index];
            if bitmap[index / 8] & (1u8 << (index % 8)) == 0 {
                self.stack.push(value);
            } else {
                let Some(items) = self.collect_items_from_iterable(value)? else {
                    let rendered = value.to_string(&self.heap);
                    return Err(self
                        .throw(
                            TypeError,
                            &format!("Cannot unpack non-iterable {rendered}."),
                        )
                        .unwrap_err());
                };
                self.stack.extend_from_slice(&items);
            }
        }
        let total = self.stack.len() - result_base;
        self.stack.drain(segment_base..result_base);
        u8::try_from(total).map_err(|_| {
            self.throw(TypeError, "A call cannot pass more than 255 arguments.")
                .unwrap_err()
        })
    }

    /// The "wrong number of arguments" message: "at least N" for a variadic
    /// function, an exact count when there are no optionals, a range otherwise.
    fn arity_message(required: usize, arity: usize, is_variadic: bool, got: usize) -> String {
        let plural = |n: usize| if n == 1 { "" } else { "s" };
        if is_variadic {
            format!(
                "Expected at least {required} argument{} but got {got}.",
                plural(required)
            )
        } else if required == arity {
            format!("Expected {arity} argument{} but got {got}.", plural(arity))
        } else {
            format!("Expected between {required} and {arity} arguments but got {got}.")
        }
    }

    /// Execute a call to a native function.
    ///
    /// Checks that the number of arguments matches to the arity of the function.
    /// After the call the stack is truncated to remove the arguments and the function
    /// and the result is pushed onto the stack.
    #[allow(clippy::branches_sharing_code)]
    fn execute_native_function_call(&mut self, f: NativeFunctionId, arg_count: u8) -> VmResult {
        let f = f.to_value(&self.heap);
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            let message = if arity.len() == 1 {
                format!(
                    "Native function '{}' expected {} argument{}, got {}.",
                    f.name.to_value(&self.heap),
                    arity[0],
                    { if arity[0] == 1 { "" } else { "s" } },
                    arg_count
                )
            } else {
                format!(
                    "Native function '{}' expected any of {:?} arguments, got {}.",
                    f.name.to_value(&self.heap),
                    arity,
                    arg_count
                )
            };
            return self.throw(TypeError, &message);
        }
        let fun = f.fun;
        let start_index = self.stack.len() - usize::from(arg_count);
        let args: TinyVec<[Value; INLINE_NATIVE_ARGS]> = self.stack[start_index..].into();
        let value = fun(self, &args)?;
        self.stack.truncate(start_index - 1);
        self.stack_push(value);
        Ok(None)
    }

    /// Execute a call to a native method.
    ///
    /// Checks that the number of arguments matches to the arity of the method.
    /// After the call the stack is truncated to remove the arguments and the receiver
    /// and the result is pushed onto the stack.
    #[allow(clippy::branches_sharing_code)]
    fn execute_native_method_call(
        &mut self,
        f: NativeMethodId,
        receiver: &Value,
        arg_count: u8,
    ) -> VmResult {
        let f = f.to_value(&self.heap);
        let arity = f.arity;
        if !arity.contains(&arg_count) {
            let message = if arity.len() == 1 {
                format!(
                    "Native method '{}' of class {} expected {} argument{}, got {}.",
                    f.name.to_value(&self.heap),
                    *receiver.class_name(&self.heap).to_value(&self.heap),
                    arity[0],
                    { if arity[0] == 1 { "" } else { "s" } },
                    arg_count
                )
            } else {
                format!(
                    "Native method '{}' of class {} expected any of {:?} arguments, got {}.",
                    *f.name.to_value(&self.heap),
                    *receiver.class_name(&self.heap).to_value(&self.heap),
                    arity,
                    arg_count
                )
            };
            return self.throw(TypeError, &message);
        }
        let fun = f.fun;
        // Copied out of the heap `NativeMethod` borrow before re-entering
        // `&mut self` below (both `Copy`); mirrors `let fun = f.fun`.
        #[cfg(feature = "plugins")]
        let (plugin_fn, name) = (f.plugin_fn, f.name);
        let start_index = self.stack.len() - usize::from(arg_count);
        let args: TinyVec<[Value; INLINE_NATIVE_ARGS]> = self.stack[start_index..].into();

        // Plugin methods delegate to `call_plugin_method` (receiver passed
        // separately, `args` unchanged); both paths return a `VmResult<Value>`
        // and then apply the same stack discipline below. The receiver and args
        // stay on the stack for the whole call (truncation is afterward), so
        // they are GC-rooted while a plugin re-enters.
        #[cfg(feature = "plugins")]
        let value = if let Some(plugin_fn) = plugin_fn {
            call_plugin_method(self, plugin_fn, *receiver, &args, name)?
        } else {
            fun(self, receiver, &args)?
        };
        #[cfg(not(feature = "plugins"))]
        let value = fun(self, receiver, &args)?;

        self.stack
            .truncate(self.stack.len() - usize::from(arg_count) - 1);
        self.stack_push(value);
        Ok(None)
    }
}

// Define methods, classes, and modules, functions
impl VM {
    /// Bind a method to an instance.
    ///
    /// The instance is still on top of the stack.
    /// Returns true if the method was found and false otherwise.
    pub(super) fn bind_method(&mut self, class: Value, name: StringId) -> bool {
        let class = class.as_class();
        let Some(method) = class.to_value(&self.heap).methods.get(&name) else {
            return false;
        };
        let bound_method = Value::bound_method(
            // the instance
            *self.peek(0).expect("Buffer underflow in OP_METHOD"),
            *method,
            &mut self.heap,
        );
        self.stack.pop(); // instance
        self.stack_push(bound_method);
        true
    }

    /// Capture upvalues from the surrounding scope.
    ///
    /// Iterate over all open upvalues up to the desired index.
    /// If the the requested value has already been captured then reuse that.
    /// Otherwise create a new upvalue and insert it into the list of open upvalues.
    pub(super) fn capture_upvalue(&mut self, local: usize) -> UpvalueId {
        let local = self.callstack.current().stack_base + local;
        let mut upvalue_index = 0;
        let mut upvalue = None;

        for (i, this) in self.open_upvalues.iter().enumerate() {
            upvalue = Some(this);
            upvalue_index = i;
            if this.to_value(&self.heap).as_open() <= local {
                break;
            }
        }

        if let Some(upvalue) = upvalue
            && upvalue.to_value(&self.heap).as_open() == local
        {
            return *upvalue;
        }
        let upvalue = self.heap.add_upvalue(Upvalue::Open(local));
        let upvalue_id = upvalue.upvalue_location();
        self.open_upvalues.insert(upvalue_index, *upvalue_id);

        *upvalue_id
    }

    /// Close the upvalue from the specified position.
    ///
    /// This is used to close upvalues when a their defining scope ends
    /// and they are still captured by a closure.
    pub(super) fn close_upvalue(&mut self, last: usize) {
        while self
            .open_upvalues
            .front()
            .is_some_and(|v| v.to_value(&self.heap).as_open() >= last)
        {
            let upvalue = self
                .open_upvalues
                .pop_front()
                .unwrap()
                .to_value_mut(&mut self.heap);

            let pointed_value = self.stack[upvalue.as_open()];
            *upvalue = Upvalue::Closed(pointed_value);
        }
    }

    pub(super) fn define_method(&mut self, method_name: StringId) {
        let method = *self.peek(0).expect("Stack underflow in OP_METHOD");
        let class = *self
            .peek_mut(1)
            .expect("Stack underflow in OP_METHOD")
            .as_class_mut();
        class
            .to_value_mut(&mut self.heap)
            .methods
            .insert(method_name, method);
        self.stack.pop();
    }

    /// Record a class variable on the class being defined. The stack holds
    /// `(class, annotation, default)`; the two values are popped, the class
    /// stays for the rest of the body.
    pub(super) fn define_class_variable(&mut self, variable_name: StringId, has_default: bool) {
        let default = self
            .stack
            .pop()
            .expect("Stack underflow in OP_CLASS_VARIABLE");
        let annotation = self
            .stack
            .pop()
            .expect("Stack underflow in OP_CLASS_VARIABLE");
        let class = *self
            .peek_mut(0)
            .expect("Stack underflow in OP_CLASS_VARIABLE")
            .as_class_mut();
        class.to_value_mut(&mut self.heap).add_class_variable(
            variable_name,
            annotation,
            has_default.then_some(default),
        );
    }
}

// Return
impl VM {
    /// Return from the current module or function.
    ///
    /// If the current frame is a module then the module is imported into the
    /// current module. If the current module is the main script that we return
    /// from the main loop.
    ///
    /// If the current frame is a function, we return the value and close the upvalues.
    pub(super) fn return_(&mut self) -> VmResult<Return> {
        // Pop the return value. If none was specified (empty return, missing return, module)
        // then the value is nil (or StopIteration  for return in generators). This is handled by the compiler.
        let result = self.stack.pop().expect("Stack underflow in OP_RETURN");
        let frame = self
            .callstack
            .pop(&self.heap)
            .expect("Call stack underflow in OP_RETURN");
        // We just popped the main script
        if self.callstack.is_empty() {
            self.stack.pop();
            return Ok(Return::Program(frame));
        }
        if frame.is_module {
            self.handle_module_end()?;
            return Ok(Return::Function(frame));
        }
        // Normal function return
        self.close_upvalue(frame.stack_base);
        // Pop all of the arguments and locals as well as the function itself.
        self.stack.truncate(frame.stack_base);
        self.stack_push(result);
        Ok(Return::Function(frame))
    }

    pub(super) fn yield_(&mut self) -> Return {
        let frame = self
            .callstack
            .pop(&self.heap)
            .expect("Call stack underflow in OP_RETURN");
        self.close_upvalue(frame.stack_base);
        Return::Function(frame)
    }

    fn handle_module_end(&mut self) -> VmResult {
        // Pop the module itself from the stack
        self.stack.pop();
        let last_module = self.modules.pop().expect("Module underflow in OP_RETURN");
        // The completed import is cached by identity: a later import of the
        // same module binds this module object instead of re-executing it.
        self.module_cache
            .insert(last_module.to_value(&self.heap).path.clone(), last_module);
        let last_module_alias = last_module.to_value(&self.heap).alias;
        let names_to_import =
            std::mem::take(&mut last_module.to_value_mut(&mut self.heap).names_to_import);
        let was_local_import = last_module.to_value(&self.heap).local_import;
        if let Some(names) = names_to_import {
            for name in names {
                let Some(value) = last_module.to_value(&self.heap).globals.get(&name).copied()
                else {
                    let message = format!(
                        "Could not find name to import `{}`.",
                        name.to_value(&self.heap)
                    );
                    return Err(self.throw(ImportError, &message).unwrap_err());
                };
                if was_local_import {
                    self.stack_push(value.value);
                } else {
                    self.defining_globals_mut().insert(name, value);
                }
            }
        } else if was_local_import {
            self.stack_push(last_module.into());
        } else {
            self.defining_globals_mut().insert(
                last_module_alias,
                Global {
                    value: last_module.into(),
                    mutable: true,
                },
            );
        }
        let script_name = self.heap.builtin_constants().script_name;
        let module_name: Value = self
            .modules
            .last()
            .expect("Module underflow in OP_RETURN")
            .to_value(&self.heap)
            .name
            .into();
        self.globals_mut().insert(
            script_name,
            Global {
                value: module_name,
                mutable: true,
            },
        );

        Ok(None)
    }
}

impl VM {
    fn get_proxy_class(&self, value: Value) -> Option<ClassId> {
        match value {
            Value::String(_) => Some(get_native_class_id(&self.heap, "String")),
            _ => None,
        }
    }
}

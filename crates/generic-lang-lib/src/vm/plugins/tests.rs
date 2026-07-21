//! Unit tests for the plugin host: value bridging, every host callback
//! against a real VM, the trampoline, and `call_plugin`'s result mapping.
//!
//! These run under miri too — the blob bridge (`to_ffi`/`from_ffi`) is
//! sound: `GenericValue`'s limbs are `MaybeUninit`, so bit-copying a whole
//! `Value` (whose small variants leave most of the 32 bytes uninitialized)
//! never asserts those bytes are initialized. Only the two loader tests are
//! `#[cfg(not(miri))]`, because they do real filesystem and `dlopen` work
//! miri cannot execute.
//!
//! Tests exercise the vtable exactly like a plugin would — raw pointers
//! included.
//!
//! Ordering invariant: `build_host_api` stashes a raw pointer to the `VM`
//! in `ctx`, so a test must do all its `&mut vm` work (allocation, instance
//! construction) *before* building the vtable and must not touch `vm`
//! directly again until it is done calling callbacks — otherwise the later
//! `&mut vm` invalidates the `ctx` pointer and miri (Stacked Borrows)
//! rejects the reborrow inside the callback. (`call_plugin`, which builds
//! its own vtable, is fine after the last callback use.)
#![allow(unsafe_code)]

#[cfg(not(miri))]
use std::env::consts::{DLL_PREFIX, DLL_SUFFIX};
use std::path::PathBuf;

use generic_lang_api::{FfiReturn, FfiStatus, FfiStr, GenericValue, HostApi, PluginFn, ValueKind};
use num_bigint::BigInt;

use crate::value::{
    Class, Closure, Function, Generator, GenericInt, GenericRational, Instance, ListIterator,
    Module, NativeClass, Number, Range, Tuple, Value, get_native_class_id,
};
use crate::vm::VM;
use crate::vm::callstack::CallFrame;
use crate::vm::errors::VmErrorKind;
use crate::vm::plugins::host_api::{build_host_api, from_ffi, to_ffi, value_kind_of};
#[cfg(not(miri))]
use crate::vm::plugins::loader::find_plugin_candidate;
use crate::vm::plugins::trampolines::{call_plugin, plugin_trampoline};

// --- Value constructors for the tests: build heap values the way a running
// VM holds them (interned strings, native-backed containers, a plain
// instance), and fetch a builtin from the loaded prelude. ---

fn new_string(vm: &mut VM, s: &str) -> Value {
    Value::String(vm.heap.string_id(&s))
}

/// A native-backed instance of the given container class (`List`, `Dict`,
/// `Set`, …), empty.
fn new_container(vm: &mut VM, kind: &str) -> Value {
    let class = get_native_class_id(&vm.heap, kind);
    vm.heap
        .add_instance(Instance::new(class, Some(NativeClass::new(kind))))
}

fn new_list(vm: &mut VM) -> Value {
    new_container(vm, "List")
}

fn plain_instance(vm: &mut VM) -> Value {
    let name_id = vm.heap.string_id(&"Plain");
    let class = vm.heap.add_class(Class::new(name_id, false));
    vm.heap.add_instance(Instance::new(*class.as_class(), None))
}

fn builtin_value(vm: &mut VM, name: &str) -> Value {
    let name_id = vm.heap.string_id(&name);
    vm.builtins
        .get(&name_id)
        .unwrap_or_else(|| panic!("builtin `{name}` not defined"))
        .value
}

/// Every `ValueKind`, kept honest by the exhaustive `match`: a new variant
/// in the api crate fails compilation here until the host produces it and
/// the kind test below exercises it.
fn all_value_kinds() -> [ValueKind; 21] {
    const ALL: [ValueKind; 21] = [
        ValueKind::Nil,
        ValueKind::Bool,
        ValueKind::Int,
        ValueKind::BigInt,
        ValueKind::Float,
        ValueKind::Rational,
        ValueKind::String,
        ValueKind::List,
        ValueKind::Tuple,
        ValueKind::Dict,
        ValueKind::Set,
        ValueKind::Range,
        ValueKind::StopIteration,
        ValueKind::Instance,
        ValueKind::Class,
        ValueKind::Function,
        ValueKind::Module,
        ValueKind::Exception,
        ValueKind::Generator,
        ValueKind::Iterator,
        ValueKind::Other,
    ];
    match ALL[0] {
        ValueKind::Nil
        | ValueKind::Bool
        | ValueKind::Int
        | ValueKind::BigInt
        | ValueKind::Float
        | ValueKind::Rational
        | ValueKind::String
        | ValueKind::List
        | ValueKind::Tuple
        | ValueKind::Dict
        | ValueKind::Set
        | ValueKind::Range
        | ValueKind::StopIteration
        | ValueKind::Instance
        | ValueKind::Class
        | ValueKind::Function
        | ValueKind::Module
        | ValueKind::Exception
        | ValueKind::Generator
        | ValueKind::Iterator
        | ValueKind::Other => ALL,
    }
}

// --- pure helpers (run under miri too) ---

#[test]
fn value_kind_covers_every_kind() {
    let mut vm = VM::new();

    // Numbers: a small int, a bigint that still fits an i64 (→ `Int`), and
    // one that overflows it (→ `BigInt`).
    let small_int = Value::from(42i64);
    let big_int_that_fits = vm.heap.add_big_int(BigInt::from(7));
    let big_int_too_large = vm.heap.add_big_int(BigInt::from(i64::MAX) + 1);
    let rational = Value::from(Number::Rational(
        GenericRational::new(GenericInt::Small(1), GenericInt::Small(3), &mut vm.heap).unwrap(),
    ));
    let float = Value::from(Number::Float(std::f64::consts::PI));

    // A string and the native-backed containers.
    let string = new_string(&mut vm, "hi");
    let list = new_list(&mut vm);
    let dict = new_container(&mut vm, "Dict");
    let set = new_container(&mut vm, "Set");
    let tuple_class = get_native_class_id(&vm.heap, "Tuple");
    let tuple = vm.heap.add_instance(Instance::new(
        tuple_class,
        Some(Tuple::new(vec![Value::Nil]).into()),
    ));
    let range_class = get_native_class_id(&vm.heap, "Range");
    let range = vm.heap.add_instance(Instance::new(
        range_class,
        Some(Range::new(GenericInt::Small(0), GenericInt::Small(3)).into()),
    ));

    // An exception instance, a plain user instance, a class value, and a
    // builtin function.
    let exception = vm.create_exception("ValueError", "boom");
    let instance = plain_instance(&mut vm);
    let class = Value::Class(get_native_class_id(&vm.heap, "List"));
    let function = builtin_value(&mut vm, "print");

    // A rational and a module.
    let module_name = vm.heap.string_id(&"sync");
    let module = vm.heap.add_module(Module::new(
        module_name,
        PathBuf::from("sync"),
        None,
        module_name,
        false,
    ));
    // Generator and iterator need scaffolding: a generator wraps a
    // (suspended) callframe over a closure — an empty function suffices for
    // kind classification — and the iterator drives the list built above.
    let raw_function = vm.heap.add_function(Function::new(0, module_name));
    let closure = Closure::new(*raw_function.as_function(), false, None, &vm.heap);
    let closure = vm.heap.add_closure(closure);
    let generator_data = Generator::new(
        CallFrame::from_closure_id(*closure.as_closure()),
        vec![],
        vec![],
    );
    let generator_class = get_native_class_id(&vm.heap, "Generator");
    let generator = vm
        .heap
        .add_instance(Instance::new(generator_class, Some(generator_data.into())));
    let iterator_data = ListIterator::new(*list.as_instance());
    let iterator_class = get_native_class_id(&vm.heap, "ListIterator");
    let iterator = vm
        .heap
        .add_instance(Instance::new(iterator_class, Some(iterator_data.into())));

    // Forward direction: each value classifies as its expected kind.
    let expected = [
        (Value::Nil, ValueKind::Nil),
        (Value::Bool(true), ValueKind::Bool),
        (Value::StopIteration, ValueKind::StopIteration),
        (small_int, ValueKind::Int),
        (big_int_that_fits, ValueKind::Int),
        (big_int_too_large, ValueKind::BigInt),
        (float, ValueKind::Float),
        (rational, ValueKind::Rational),
        (string, ValueKind::String),
        (list, ValueKind::List),
        (tuple, ValueKind::Tuple),
        (dict, ValueKind::Dict),
        (set, ValueKind::Set),
        (range, ValueKind::Range),
        (exception, ValueKind::Exception),
        (instance, ValueKind::Instance),
        (class, ValueKind::Class),
        (function, ValueKind::Function),
        (module, ValueKind::Module),
        (generator, ValueKind::Generator),
        (iterator, ValueKind::Iterator),
        // Raw function objects are VM-internal (not callable via
        // `call_value`) — the canonical `Other`.
        (raw_function, ValueKind::Other),
    ];
    for (value, kind) in expected {
        assert_eq!(
            value_kind_of(&vm.heap, value),
            kind as u32,
            "wrong kind for {value:?}"
        );
    }

    // Reverse direction: every kind the api crate declares must be
    // exercised above — an orphaned `ValueKind` variant fails here (and a
    // brand-new one already failed to compile in `all_value_kinds`).
    let mut exercised: Vec<u32> = expected.iter().map(|(_, kind)| *kind as u32).collect();
    exercised.sort_unstable();
    exercised.dedup();
    let mut all: Vec<u32> = all_value_kinds().iter().map(|kind| *kind as u32).collect();
    all.sort_unstable();
    assert_eq!(exercised, all, "every ValueKind must have an exemplar");
}

fn message_string(vm: &VM, ret: FfiReturn) -> String {
    match from_ffi(ret.value) {
        Value::String(id) => id.to_value(&vm.heap).clone(),
        other => panic!("expected a message string, got {other:?}"),
    }
}

/// The ok value of a callback return; panics on a nonzero status.
fn ok_value(ret: FfiReturn) -> Value {
    assert_eq!(ret.status, FfiStatus::Ok as u32, "expected a success");
    from_ffi(ret.value)
}

/// Assert `ret` carries an exception and check its class like a plugin
/// would — `builtin_get` for the class, `is_instance` for the check.
fn assert_error(api: &HostApi, ret: FfiReturn, class_name: &str) {
    assert_eq!(
        ret.status,
        FfiStatus::Exception as u32,
        "expected an exception"
    );
    let name = FfiStr {
        ptr: class_name.as_ptr(),
        len: class_name.len(),
    };
    let class = (api.builtin_get)(api.ctx, name);
    assert_eq!(class.status, 0, "missing builtin `{class_name}`");
    let is = (api.is_instance)(api.ctx, ret.value, class.value);
    assert_eq!(
        ok_value(is),
        Value::Bool(true),
        "expected an instance of {class_name}"
    );
}

/// Read a host-returned `FfiStr` the way a plugin would.
fn read_ffi_str<'a>(s: FfiStr) -> &'a str {
    assert!(!s.ptr.is_null(), "expected a string, got the null FfiStr");
    // SAFETY: a non-null FfiStr from the host references `len` valid
    // bytes of interned UTF-8 (ABI contract).
    unsafe { std::str::from_utf8(std::slice::from_raw_parts(s.ptr, s.len)).unwrap() }
}

#[test]
fn ffi_blob_round_trips() {
    // Arrange: one value from each broad category — immediates, a bigint, a
    // string, a native container, an exception, a class, and a function.
    let mut vm = VM::new();
    let values = [
        Value::Nil,
        Value::Bool(true),
        Value::StopIteration,
        Value::from(-3i64),
        Value::from(2.25f64),
        vm.heap.add_big_int(BigInt::from(i64::MAX) + 1),
        new_string(&mut vm, "round trip"),
        new_list(&mut vm),
        vm.create_exception("TypeError", "x"),
        Value::Class(get_native_class_id(&vm.heap, "List")),
        builtin_value(&mut vm, "print"),
    ];

    // to_ffi then from_ffi is the identity for every one of them.
    for value in values {
        assert_eq!(from_ffi(to_ffi(value)), value);
    }
}

#[test]
fn value_kind_and_scalar_accessors() {
    let mut vm = VM::new();
    let seven = 7;
    let fourteetwo = 42;
    let one_point_five = 1.5;
    let big_ok = vm.heap.add_big_int(BigInt::from(seven));
    let big_overflow = vm.heap.add_big_int(BigInt::from(i64::MAX) + 1);
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // value_kind: the discriminator every plugin starts from.
    assert_eq!((api.value_kind)(api.ctx, nil), ValueKind::Nil as u32);

    // bool_get: extracts the bool, false for anything else.
    // Success
    let mut target_bool = false;
    let succeeded = (api.bool_get)(api.ctx, to_ffi(Value::Bool(true)), &raw mut target_bool);
    assert!(succeeded);
    assert!(target_bool);

    // Failure
    let succeeded = (api.bool_get)(api.ctx, nil, &raw mut target_bool);
    assert!(!succeeded);

    // int_get: extracts the i64 (small ints and i64-sized bigints)...
    // Success int
    let mut n = 0i64;
    let succeeded = (api.int_get)(api.ctx, to_ffi(Value::from(fourteetwo)), &raw mut n);
    assert!(succeeded);
    assert_eq!(n, fourteetwo);

    // Success bigint
    let succeeded = (api.int_get)(api.ctx, to_ffi(big_ok), &raw mut n);
    assert!(succeeded);
    assert_eq!(n, seven);

    // ...and both its failures — oversized bigint and non-integer — are the
    // single `false` (a raw i64 payload needs the out-parameter).
    let succeeded = (api.int_get)(api.ctx, to_ffi(big_overflow), &raw mut n);
    assert!(!succeeded);

    let succeeded = (api.int_get)(api.ctx, nil, &raw mut n);
    assert!(!succeeded);

    // float_get: extracts the f64, false for anything else.
    // Success
    let mut f = 0f64;
    let succeeded = (api.float_get)(api.ctx, to_ffi(Value::from(one_point_five)), &raw mut f);
    assert!(succeeded);
    assert!((f - one_point_five).abs() < f64::EPSILON);

    // Failure
    let succeeded = (api.float_get)(api.ctx, to_ffi(Value::from(fourteetwo)), &raw mut f);
    assert!(!succeeded);
}

#[test]
fn string_interning_and_bytes() {
    // Arrange.
    let mut vm = VM::new();
    let hello = "héllo";
    let ffi = FfiStr {
        ptr: hello.as_ptr(),
        len: hello.len(),
    };
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // Interning: creating the same string twice yields the same id.
    let first = ok_value((api.string_new)(api.ctx, ffi));
    let second = ok_value((api.string_new)(api.ctx, ffi));
    assert_eq!(first, second);

    // Read-back: the interned bytes round-trip through `string_get`.
    let mut read_back = FfiStr::null();
    let succeeded = (api.string_get)(api.ctx, to_ffi(first), &raw mut read_back);
    assert!(succeeded);
    assert_eq!(read_ffi_str(read_back), hello);

    // Invalid UTF-8 is rejected as a ValueError without panicking.
    let invalid = [0xffu8, 0xfe];
    let ffi = FfiStr {
        ptr: invalid.as_ptr(),
        len: invalid.len(),
    };
    assert_error(&api, (api.string_new)(api.ctx, ffi), "ValueError");

    // `string_get` on a non-string signals "not a string".
    let mut scratch = FfiStr::null();
    let succeeded = (api.string_get)(api.ctx, nil, &raw mut scratch);
    assert!(!succeeded);
}

#[test]
fn list_construct_and_access() {
    // Arrange: a fresh, empty native list.
    let mut vm = VM::new();
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    let list = (api.list_new)(api.ctx);
    assert_eq!((api.value_kind)(api.ctx, list), ValueKind::List as u32);

    // push: appends, nil on success; TypeError on a non-list receiver.
    // Success
    assert_eq!(
        ok_value((api.list_push)(api.ctx, list, to_ffi(Value::from(1i64)))),
        Value::Nil
    );
    assert_eq!(
        ok_value((api.list_push)(api.ctx, list, to_ffi(Value::from(2i64)))),
        Value::Nil
    );
    //Failure
    assert_error(&api, (api.list_push)(api.ctx, nil, nil), "TypeError");

    // len: reflects the two pushes; false on a non-list.
    // Success
    let mut len = 0usize;
    let succeeded = (api.list_len)(api.ctx, list, &raw mut len);
    assert!(succeeded);
    assert_eq!(len, 2);
    // Failure
    let succeeded = (api.list_len)(api.ctx, nil, &raw mut len);
    assert!(!succeeded);

    // get: the element; IndexError out of bounds, TypeError on a non-list.
    // Success
    let ret = (api.list_get)(api.ctx, list, 1);
    assert_eq!(ok_value(ret), Value::from(2i64));
    // Failure: Out of bounds
    let ret = (api.list_get)(api.ctx, list, 2);
    assert_error(&api, ret, "IndexError");
    // Failure: Wrong type
    let ret = (api.list_get)(api.ctx, nil, 0);
    assert_error(&api, ret, "TypeError");

    // set: replaces in place; IndexError out of bounds, TypeError non-list.
    // Success
    let ret = (api.list_set)(api.ctx, list, 0, to_ffi(Value::Bool(true)));
    assert_eq!(ok_value(ret), Value::Nil);
    let ret = (api.list_get)(api.ctx, list, 0);
    assert_eq!(ok_value(ret), Value::Bool(true));
    // Failure: Out of bounds
    let ret = (api.list_set)(api.ctx, list, 9, nil);
    assert_error(&api, ret, "IndexError");
    // Failure: Wrong type
    let ret = (api.list_set)(api.ctx, nil, 0, nil);
    assert_error(&api, ret, "TypeError");
}

#[test]
fn dict_and_set_operations() {
    // Arrange: an empty dict and set, plus a string value to store.
    let mut vm = VM::new();
    let dict = new_container(&mut vm, "Dict");
    let set = new_container(&mut vm, "Set");
    let value_x = new_string(&mut vm, "x");
    let key = to_ffi(Value::from(1i64));
    let missing = to_ffi(Value::from(2i64));
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // dict: set, then get and contains observe it.
    let ret = (api.dict_set)(api.ctx, to_ffi(dict), key, to_ffi(value_x));
    assert_eq!(ret.status, 0);
    let ret = (api.dict_get)(api.ctx, to_ffi(dict), key);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), value_x);
    let ret = (api.dict_contains)(api.ctx, to_ffi(dict), key);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), Value::Bool(true));

    // dict errors: a missing key is a KeyError, a non-dict a TypeError —
    // both handed over, never left pending.
    let ret = (api.dict_get)(api.ctx, to_ffi(dict), missing);
    assert_error(&api, ret, "KeyError");
    let ret = (api.dict_get)(api.ctx, nil, key);
    assert_error(&api, ret, "TypeError");

    // set: add, then contains is true for the item, false for another.
    let item = to_ffi(Value::from(9i64));
    let ret = (api.set_add)(api.ctx, to_ffi(set), item);
    assert_eq!(ret.status, 0);
    let ret = (api.set_contains)(api.ctx, to_ffi(set), item);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), Value::Bool(true));
    let ret = (api.set_contains)(api.ctx, to_ffi(set), missing);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), Value::Bool(false));

    // These re-entering callbacks must leave the VM stack as they found it.
    assert_eq!(vm.stack.len(), 0, "callbacks must be stack-neutral");
}

#[test]
fn attribute_access() {
    // Arrange: a plain instance, a value to store, and the field name.
    let mut vm = VM::new();
    let instance = plain_instance(&mut vm);
    let value = new_string(&mut vm, "v");
    let field = "field";
    let name = FfiStr {
        ptr: field.as_ptr(),
        len: field.len(),
    };
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // A missing field is an AttributeError.
    let ret = (api.attr_get)(api.ctx, to_ffi(instance), name);
    assert_error(&api, ret, "AttributeError");

    // set, then get and has observe the field.
    let ret = (api.attr_set)(api.ctx, to_ffi(instance), name, to_ffi(value));
    assert_eq!(ret.status, 0);
    let ret = (api.attr_get)(api.ctx, to_ffi(instance), name);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), value);
    let ret = (api.attr_has)(api.ctx, to_ffi(instance), name);
    assert_eq!(ok_value(ret), Value::Bool(true));

    // Non-instances have no attributes — get and has both TypeError.
    let ret = (api.attr_get)(api.ctx, nil, name);
    assert_error(&api, ret, "TypeError");
    let ret = (api.attr_has)(api.ctx, nil, name);
    assert_error(&api, ret, "TypeError");
}

#[test]
fn display_is_raw_and_str_honors_dunder() {
    // Arrange: an exception whose `__str__` differs from its raw repr.
    let mut vm = VM::new();
    let exception = vm.create_exception("ValueError", "boom");
    let api = build_host_api(&mut vm);

    // value_display: the raw repr — does NOT run `__str__`.
    let displayed = (api.value_display)(api.ctx, to_ffi(exception));
    let mut ffi = FfiStr::null();
    let succeeded = (api.string_get)(api.ctx, displayed, &raw mut ffi);
    assert!(succeeded);
    assert_eq!(read_ffi_str(ffi), "ValueError('boom')");

    // value_str: re-enters and runs `__str__`, which returns only the
    // message, and leaves the VM stack clean.
    let ret = (api.value_str)(api.ctx, to_ffi(exception));
    assert_eq!(ret.status, 0);
    assert_eq!(message_string(&vm, ret), "boom");
    assert_eq!(vm.stack.len(), 0, "re-entering callback must clean up");
}

#[test]
fn truthiness_equality_hash() {
    // Arrange: two equal-but-distinct big integers — separate heap
    // allocations (bigints are not deduped) so the equality/hash path runs
    // over different handles rather than short-circuiting on identity.
    let mut vm = VM::new();
    let a = vm.heap.add_big_int(BigInt::from(7));
    let b = vm.heap.add_big_int(BigInt::from(7));
    assert_ne!(a, b, "the two bigints must be distinct handles");
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // value_truthy: false is falsey, a nonzero int is truthy.
    let ret = (api.value_truthy)(api.ctx, to_ffi(Value::Bool(false)));
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), Value::Bool(false));
    let ret = (api.value_truthy)(api.ctx, to_ffi(Value::from(1i64)));
    assert_eq!(from_ffi(ret.value), Value::Bool(true));

    // value_equals: the two distinct-but-equal bigints compare equal, an
    // unequal kind does not.
    let ret = (api.value_equals)(api.ctx, to_ffi(a), to_ffi(b));
    assert_eq!(from_ffi(ret.value), Value::Bool(true));
    let ret = (api.value_equals)(api.ctx, to_ffi(a), nil);
    assert_eq!(from_ffi(ret.value), Value::Bool(false));

    // value_hash: equal values hash equal.
    let hash_a = (api.value_hash)(api.ctx, to_ffi(a));
    let hash_b = (api.value_hash)(api.ctx, to_ffi(b));
    assert_eq!(hash_a.status, 0);
    assert_eq!(from_ffi(hash_a.value), from_ffi(hash_b.value));
}

#[test]
fn call_value_happy_and_error_paths() {
    // Arrange: the builtin `str` native and a one-int argument list.
    let mut vm = VM::new();
    let str_fn = builtin_value(&mut vm, "str");
    let args = [to_ffi(Value::from(42i64))];
    let nil = to_ffi(Value::Nil);
    let api = build_host_api(&mut vm);

    // Happy path: `str(42)` returns "42".
    let ret = (api.call_value)(api.ctx, to_ffi(str_fn), args.as_ptr(), args.len());
    assert_eq!(ret.status, 0);
    assert_eq!(message_string(&vm, ret), "42");

    // Arity mismatch throws inside the VM and comes back as an exception.
    let two = [nil, nil];
    let ret = (api.call_value)(api.ctx, to_ffi(str_fn), two.as_ptr(), two.len());
    assert_error(&api, ret, "TypeError");

    // Uncallable values throw TypeError; the exception is handed to the
    // plugin, not left pending, and the VM is left clean.
    let ret = (api.call_value)(api.ctx, nil, args.as_ptr(), args.len());
    assert_error(&api, ret, "TypeError");
    assert_eq!(vm.stack.len(), 0);
    assert_eq!(vm.callstack.len(), 0);
}

#[test]
fn invoke_method_on_native_receiver() {
    // Arrange: an empty list and a one-int argument list.
    let mut vm = VM::new();
    let list = new_list(&mut vm);
    let args = [to_ffi(Value::from(5i64))];
    let append = "append";
    let append = FfiStr {
        ptr: append.as_ptr(),
        len: append.len(),
    };
    let missing = "missing";
    let missing = FfiStr {
        ptr: missing.as_ptr(),
        len: missing.len(),
    };
    let api = build_host_api(&mut vm);

    // Happy path: `list.append(5)` runs and grows the list.
    let ret = (api.invoke_method)(api.ctx, to_ffi(list), append, args.as_ptr(), args.len());
    assert_eq!(ret.status, 0);
    let mut len = 0usize;
    let succeeded = (api.list_len)(api.ctx, to_ffi(list), &raw mut len);
    assert!(succeeded);
    assert_eq!(len, 1);

    // Unknown methods surface as AttributeError, VM left clean.
    let ret = (api.invoke_method)(api.ctx, to_ffi(list), missing, args.as_ptr(), args.len());
    assert_error(&api, ret, "AttributeError");
    assert_eq!(vm.stack.len(), 0);
}

#[test]
fn rooting_pushes_and_pops_the_vm_stack() {
    // Arrange.
    let mut vm = VM::new();
    let value = new_string(&mut vm, "rooted");
    let api = build_host_api(&mut vm);

    // root pushes onto the VM stack; unroot pops.
    (api.root)(api.ctx, to_ffi(value));
    (api.root)(api.ctx, to_ffi(value));
    assert_eq!(vm.stack.len(), 2);
    (api.unroot)(api.ctx, 1);
    assert_eq!(vm.stack.len(), 1);

    // Over-unrooting saturates at empty instead of panicking.
    (api.unroot)(api.ctx, 5);
    assert_eq!(vm.stack.len(), 0);
}

// --- trampolines and call_plugin result mapping ---

extern "C" fn plugin_returns_seven(
    _host: *const HostApi,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 0,
        value: to_ffi(Value::from(7i64)),
    }
}

extern "C" fn plugin_adds_args(
    host: *const HostApi,
    args: *const GenericValue,
    nargs: usize,
) -> FfiReturn {
    let api = unsafe { &*host };
    let mut sum = 0i64;
    for i in 0..nargs {
        let mut n = 0i64;
        let succeeded = (api.int_get)(api.ctx, unsafe { *args.add(i) }, &raw mut n);
        assert!(succeeded);
        sum += n;
    }
    FfiReturn {
        status: 0,
        value: to_ffi(Value::from(sum)),
    }
}

/// Returns a plain string under `STATUS_EXCEPTION` — a plugin bug; used
/// below to check the defensive `TypeError`.
extern "C" fn plugin_invalid_exception_value(
    host: *const HostApi,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    let api = unsafe { &*host };
    let message = "bad type";
    let value = (api.string_new)(
        api.ctx,
        FfiStr {
            ptr: message.as_ptr(),
            len: message.len(),
        },
    );
    assert_eq!(value.status, 0);
    FfiReturn {
        status: FfiStatus::Exception as u32,
        value: value.value,
    }
}

extern "C" fn plugin_throws_type_error(
    host: *const HostApi,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    let api = unsafe { &*host };
    let class_name = "TypeError";
    let class = (api.builtin_get)(
        api.ctx,
        FfiStr {
            ptr: class_name.as_ptr(),
            len: class_name.len(),
        },
    );
    assert_eq!(class.status, 0);
    let message = "bad type";
    let exception = (api.exception_new)(
        api.ctx,
        class.value,
        FfiStr {
            ptr: message.as_ptr(),
            len: message.len(),
        },
    );
    assert_eq!(exception.status, 0);
    FfiReturn {
        status: FfiStatus::Exception as u32,
        value: exception.value,
    }
}

/// Rethrows its first argument — how a plugin re-raises an exception it
/// caught from a re-entering callback.
extern "C" fn plugin_rethrows_arg(
    _host: *const HostApi,
    args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: FfiStatus::Exception as u32,
        value: unsafe { *args },
    }
}

extern "C" fn plugin_unknown_code(
    _host: *const HostApi,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: 999,
        value: to_ffi(Value::Nil),
    }
}

extern "C" fn plugin_fatal(
    _host: *const HostApi,
    _args: *const GenericValue,
    _nargs: usize,
) -> FfiReturn {
    FfiReturn {
        status: FfiStatus::Fatal as u32,
        value: to_ffi(Value::Nil),
    }
}

/// The class name of the pending exception on the stack top; pops it.
fn pop_pending_exception_class(vm: &mut VM) -> String {
    let exception = vm.stack.pop().expect("pending exception missing");
    exception
        .as_instance()
        .to_value(&vm.heap)
        .class
        .to_value(&vm.heap)
        .name
        .to_value(&vm.heap)
        .clone()
}

/// A heap native wrapping a plugin function — the same `add_plugin_native`
/// the loader uses per export, so the two can't drift.
fn plugin_native(vm: &mut VM, name: &str, arity: &'static [u8], fun: PluginFn) -> Value {
    let name_id = vm.heap.string_id(&name);
    vm.add_plugin_native(name_id, arity, fun)
}

/// The trampoline's own job, in isolation. Given the dispatch-site layout
/// (callee at `stack[len - argc - 1]`), it recovers that native's
/// `plugin_fn` pointer and calls it. Two *different* natives share the one
/// `plugin_trampoline`, so running both proves each routes to its own
/// pointer with no cross-talk — the heart of the "pointer on the callee"
/// design. This drives the trampoline directly with a hand-built stack;
/// [`plugin_native_dispatches_through_the_real_call_path`] is the
/// counterpart proving the *real* dispatch actually produces this layout.
#[test]
fn trampoline_recovers_the_pointer_from_the_callee() {
    // Arrange: two plugin natives wrapping different function pointers.
    let mut vm = VM::new();
    let seven = plugin_native(&mut vm, "seven", &[0], plugin_returns_seven);
    let adder = plugin_native(&mut vm, "add", &[2], plugin_adds_args);

    // No-arg native: with the callee on the stack (the dispatch-site
    // layout), the trampoline finds its pointer and calls it.
    vm.stack.push(seven);
    let result = plugin_trampoline(&mut vm, &[]).unwrap();
    assert_eq!(result, Value::from(7i64));
    vm.stack.pop();

    // Two-arg native: arguments travel zero-copy and each native routes to
    // its own pointer (no cross-talk with the first).
    let args = [Value::from(20i64), Value::from(22i64)];
    vm.stack.push(adder);
    vm.stack.extend_from_slice(&args);
    let result = plugin_trampoline(&mut vm, &args).unwrap();
    assert_eq!(result, Value::from(42i64));
    vm.stack.clear();
}

/// The integration counterpart to
/// [`trampoline_recovers_the_pointer_from_the_callee`]. Instead of
/// hand-building the stack, it drives a plugin native through the real path
/// (`call_value` → `execute_native_function_call` → trampoline), proving
/// the dispatch site genuinely parks the callee where the trampoline
/// expects — the invariant the unit test only *assumes* — and that arity is
/// gated at the dispatch site before the trampoline ever runs.
#[test]
fn plugin_native_dispatches_through_the_real_call_path() {
    let mut vm = VM::new();
    let adder = plugin_native(&mut vm, "add", &[2], plugin_adds_args);
    let api = build_host_api(&mut vm);

    // Happy path: the real dispatch places `adder` below its args, the
    // trampoline recovers the pointer, and `20 + 22` comes back as 42.
    let args = [to_ffi(Value::from(20i64)), to_ffi(Value::from(22i64))];
    let ret = (api.call_value)(api.ctx, to_ffi(adder), args.as_ptr(), 2);
    assert_eq!(ret.status, 0);
    assert_eq!(from_ffi(ret.value), Value::from(42i64));

    // Wrong arity is caught by the dispatch site before the trampoline.
    let ret = (api.call_value)(api.ctx, to_ffi(adder), args.as_ptr(), 1);
    assert_error(&api, ret, "TypeError");
    assert_eq!(vm.stack.len(), 0);
}

#[test]
fn call_plugin_maps_every_status() {
    let mut vm = VM::new();
    let name = vm.heap.string_id(&"test_plugin_fn");

    // Ok → the returned value.
    assert_eq!(
        call_plugin(&mut vm, plugin_returns_seven, &[], name).unwrap(),
        Value::from(7i64)
    );

    // A typed throw (instance built via `exception_new`) becomes a
    // pending exception of the matching class.
    let error = call_plugin(&mut vm, plugin_throws_type_error, &[], name).unwrap_err();
    assert!(matches!(error, VmErrorKind::Exception(_)));
    assert_eq!(pop_pending_exception_class(&mut vm), "TypeError");

    // A non-exception value under STATUS_EXCEPTION is a plugin bug and
    // surfaces as the raise-validation TypeError, not a crash.
    let error = call_plugin(&mut vm, plugin_invalid_exception_value, &[], name).unwrap_err();
    assert!(matches!(error, VmErrorKind::Exception(_)));
    assert_eq!(pop_pending_exception_class(&mut vm), "TypeError");

    // Unknown statuses fall back to the base Exception.
    let error = call_plugin(&mut vm, plugin_unknown_code, &[], name).unwrap_err();
    assert!(matches!(error, VmErrorKind::Exception(_)));
    assert_eq!(pop_pending_exception_class(&mut vm), "Exception");

    // STATUS_FATAL comes back as an uncatchable runtime error — no
    // pending exception is created.
    let error = call_plugin(&mut vm, plugin_fatal, &[], name).unwrap_err();
    assert!(matches!(error, VmErrorKind::Runtime(_)));
    assert_eq!(vm.stack.len(), 0);
}

#[test]
fn rethrown_exceptions_keep_their_identity() {
    // A plugin that rethrows an exception it received re-raises the
    // exact instance: class (user-defined subclasses included), fields,
    // and stack trace all survive the FFI round-trip.
    let mut vm = VM::new();
    let type_error = *builtin_value(&mut vm, "TypeError").as_class();
    let message_id = vm.heap.string_id(&"original");

    // A user-defined subclass of TypeError.
    let my_error_name_id = vm.heap.string_id(&"MyError");
    let my_error_class = vm.heap.add_class(Class::new(my_error_name_id, false));
    let my_error_class_id = *my_error_class.as_class();
    my_error_class_id.to_value_mut(&mut vm.heap).superclass = Some(type_error);
    let my_error_exception = vm.create_exception_with_class(my_error_class_id, Some(message_id));
    let nil = to_ffi(Value::Nil);
    let name = "TypeError";
    let api = build_host_api(&mut vm);

    // A plugin can catch it exactly like a generic `catch TypeError`
    // clause would...
    // Get TypeError
    let type_error_class = (api.builtin_get)(
        api.ctx,
        FfiStr {
            ptr: name.as_ptr(),
            len: name.len(),
        },
    );
    assert_eq!(type_error_class.status, 0);

    // Check if an instance of MyError is an instance of TypeError
    let ret = (api.is_instance)(api.ctx, to_ffi(my_error_exception), type_error_class.value);
    assert_eq!(ok_value(ret), Value::Bool(true));

    // ...while values of other kinds are simply not instances
    let ret = (api.is_instance)(api.ctx, nil, type_error_class.value);
    assert_eq!(ok_value(ret), Value::Bool(false));

    // and a non-class second value throws.
    let ret = (api.is_instance)(api.ctx, to_ffi(my_error_exception), nil);
    assert_error(&api, ret, "TypeError");

    let name_id = vm.heap.string_id(&"plugin_rethrows_arg");
    let error =
        call_plugin(&mut vm, plugin_rethrows_arg, &[my_error_exception], name_id).unwrap_err();
    assert!(matches!(error, VmErrorKind::Exception(_)));
    let pending = *vm.stack.last().expect("pending exception missing");
    assert!(pending.is(&my_error_exception), "must be the same instance");
    assert_eq!(pop_pending_exception_class(&mut vm), "MyError");
}

/// `is_instance` on value-type values (bool/int/float/string/rational),
/// which aren't heap instances: each matches its builtin *proxy* class
/// exactly, mirroring the `isinstance` builtin.
#[test]
fn is_instance_matches_value_type_proxy_classes() {
    // Arrange: a value per proxied kind, plus each proxy class.
    let mut vm = VM::new();
    let string = new_string(&mut vm, "hi");
    let rational = Value::from(Number::Rational(
        GenericRational::new(GenericInt::Small(1), GenericInt::Small(3), &mut vm.heap).unwrap(),
    ));
    let integer_class = builtin_value(&mut vm, "Integer");
    let float_class = builtin_value(&mut vm, "Float");
    let bool_class = builtin_value(&mut vm, "Bool");
    let string_class = builtin_value(&mut vm, "String");
    let rational_class = builtin_value(&mut vm, "Rational");
    let api = build_host_api(&mut vm);

    // Each value-type value is an instance of its own proxy class.
    for (value, class) in [
        (Value::from(5i64), integer_class),
        (Value::from(1.5f64), float_class),
        (Value::Bool(true), bool_class),
        (string, string_class),
        (rational, rational_class),
    ] {
        let ret = (api.is_instance)(api.ctx, to_ffi(value), to_ffi(class));
        assert_eq!(ok_value(ret), Value::Bool(true));
    }

    // The match is exact, not "any number": an int is not a Float.
    let ret = (api.is_instance)(api.ctx, to_ffi(Value::from(5i64)), to_ffi(float_class));
    assert_eq!(ok_value(ret), Value::Bool(false));
}

// Real filesystem access: not runnable under `cargo miri test` (no
// isolation-disable).
#[cfg(not(miri))]
#[test]
fn plugin_candidate_resolution() {
    // Arrange: an empty temp dir and the import path within it.
    let dir =
        std::env::temp_dir().join(format!("generic-plugin-candidates-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let import_path = dir.join("demo");

    // No candidate file yet → no match.
    assert!(find_plugin_candidate(&import_path, "demo").is_none());

    // The lib-prefixed candidate is found on platforms with a dylib prefix.
    let prefixed = dir.join(format!("{DLL_PREFIX}demo{DLL_SUFFIX}"));
    std::fs::write(&prefixed, b"").unwrap();
    let found = find_plugin_candidate(&import_path, "demo");
    assert_eq!(found, Some(prefixed));

    // The unprefixed candidate is preferred once it also exists.
    let unprefixed = dir.join(format!("demo{DLL_SUFFIX}"));
    std::fs::write(&unprefixed, b"").unwrap();
    let found = find_plugin_candidate(&import_path, "demo");
    assert_eq!(found, Some(unprefixed));

    std::fs::remove_dir_all(&dir).unwrap();
}

// Real filesystem + `dlopen`: not runnable under miri.
#[cfg(not(miri))]
#[test]
fn corrupt_dylib_is_an_import_error_not_a_crash() {
    // Arrange: a file with a plugin's name that is not a valid dylib.
    let dir = std::env::temp_dir().join(format!("generic-plugin-corrupt-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let dylib = dir.join(format!("demo{DLL_SUFFIX}"));
    std::fs::write(&dylib, b"this is not a shared library").unwrap();

    // Importing it claims the plugin arm (the candidate exists) but fails
    // to load — a catchable ImportError, not a crash.
    let mut vm = VM::new();
    let name_id = vm.heap.string_id(&"demo");
    let result = vm
        .try_import_plugin(&dir.join("demo"), name_id, None, None, false)
        .expect("candidate file exists, the plugin arm must claim the import");
    let error = result.unwrap_err();
    assert!(matches!(error, VmErrorKind::Exception(_)));
    assert_eq!(pop_pending_exception_class(&mut vm), "ImportError");

    std::fs::remove_dir_all(&dir).unwrap();
}

//! Rust test-plugin fixture - the primary end-to-end exercise of the generic
//! plugin ABI. Built by the Makefile `plugin-test-fixture` step and copied to
//! `test/plugin/rust/rust_demo_plugin.<ext>`, where the `.gen` tests import it
//! as `rust_demo_plugin`.
//!
//! Every function here is also a worked example for the plugin-authoring docs:
//! the happy paths, each error channel, re-entering callbacks, the rooting
//! contract, and the GC interaction all have a dedicated export.

// Every export must have the `RustPluginFn` signature
// `fn(&mut Host, &[GenericValue]) -> Result<GenericValue, PluginError>`, so
// even always-succeeding functions keep the `Result` wrapper. The numeric
// demos cast between `i64` and `f64` on purpose.
#![allow(
    clippy::unnecessary_wraps,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation
)]

use core::ffi::c_void;

use generic_lang_api::{ArgValue, GenericValue, Host, PluginError, PluginVisitFn};

// --- happy paths ---------------------------------------------------------

/// `add(a, b)` - numeric addition over the int/float mixes. Demonstrates
/// decoding arguments and returning constructed values.
fn add(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    match (host.decode(args[0]), host.decode(args[1])) {
        (ArgValue::Int(a), ArgValue::Int(b)) => Ok(host.make_int(a + b)),
        (ArgValue::Int(a), ArgValue::Float(b)) => Ok(host.make_float(a as f64 + b)),
        (ArgValue::Float(a), ArgValue::Int(b)) => Ok(host.make_float(a + b as f64)),
        (ArgValue::Float(a), ArgValue::Float(b)) => Ok(host.make_float(a + b)),
        _ => Err(host.type_error("add expects two numbers")),
    }
}

/// `shout(s)` - uppercase a string and append `!`. Demonstrates borrowing a
/// host string and interning a new one.
fn shout(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let Some(s) = host.as_str(args[0]) else {
        return Err(host.type_error("shout expects a string"));
    };
    let loud = format!("{}!", s.to_uppercase());
    Ok(host.make_str(&loud))
}

/// `sum(list)` - sum a list of numbers. Straight-line code: `list_get` never
/// re-enters, so no rooting is needed even though we allocate the result.
fn sum(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let Some(len) = host.list_len(args[0]) else {
        return Err(host.type_error("sum expects a list"));
    };
    let mut total = 0f64;
    let mut all_int = true;
    for i in 0..len {
        let item = host.list_get(args[0], i)?;
        match host.decode(item) {
            ArgValue::Int(n) => total += n as f64,
            ArgValue::Float(f) => {
                total += f;
                all_int = false;
            }
            _ => return Err(host.type_error("sum expects a list of numbers")),
        }
    }
    if all_int {
        Ok(host.make_int(total as i64))
    } else {
        Ok(host.make_float(total))
    }
}

/// `identity(v)` - return the argument unchanged. Round-trips every value
/// kind through the FFI for the value-integrity tests.
const fn identity(_host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    Ok(args[0])
}

/// `kind_name(v)` - the [`ValueKind`](generic_lang_api::ValueKind) of a value
/// as a string. Lets `.gen` tests assert the host's kind classification.
fn kind_name(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = format!("{:?}", host.kind(args[0]));
    Ok(host.make_str(&name))
}

// --- error channels ------------------------------------------------------

/// `raise(class_name, message)` - throw a fresh instance of the named builtin
/// exception class. Exercises every exception kind through one export and the
/// `builtin_get` + `exception_new` path (works for user classes too).
fn raise(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let (Some(class_name), Some(message)) = (
        host.as_str(args[0]),
        host.as_str(args[1]).map(str::to_owned),
    ) else {
        return Err(host.type_error("raise expects (class_name, message) strings"));
    };
    let class = host.builtin(class_name)?;
    Err(PluginError::Exception(
        host.make_exception(class, &message)?,
    ))
}

/// `throw_type(message)` - typed-constructor form; caught by `catch TypeError`.
fn throw_type(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let msg = host.as_str(args[0]).unwrap_or("type error").to_owned();
    Err(host.type_error(&msg))
}

/// `boom()` - panic. The `export_module!` glue catches it and turns it into a
/// catchable base `Exception` instead of aborting the interpreter process.
///
/// The default panic hook still prints the panic to stderr *before* the
/// unwind is caught. A plugin that wants silence installs its own hook; the
/// fixture does so here so the caught panic leaves no stray stderr for the
/// `.gen` test runner (which treats any unexpected stderr as a failure).
fn boom(_host: &mut Host, _args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    use std::sync::Once;
    static QUIET: Once = Once::new();
    QUIET.call_once(|| std::panic::set_hook(Box::new(|_| {})));
    panic!("intentional plugin panic");
}

// --- re-entering callbacks ----------------------------------------------

/// `call_with_21_and_double(f)` - call a generic callable with `21` and double the result.
/// Exceptions raised by `f` propagate through the plugin unchanged (via `?`).
fn call_with_21_and_double(
    host: &mut Host,
    args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    let arg = host.make_int(21);
    let result = host.call(args[0], &[arg])?;
    let Some(n) = host.as_int(result) else {
        return Err(host.type_error("call_with_21_and_double callback must return an int"));
    };
    Ok(host.make_int(n * 2))
}

/// `describe(x)` - string form honoring a user class's `__str__`
/// (re-entering). Prefixes `describe:` so tests can see the plugin ran.
fn describe(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let text = host.to_str(args[0])?; // honors __str__; re-enters
    let owned = host.as_str(text).unwrap_or_default().to_owned();
    Ok(host.make_str(&format!("describe:{owned}")))
}

/// `get_field(instance, name)` - read a field via `attr_get` (never re-enters).
fn get_field(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = host
        .as_str(args[1])
        .ok_or_else(|| host.type_error("get_field name must be a string"))?
        .to_owned();
    host.attr_get(args[0], &name)
}

/// `call_method(instance, name, arg)` - invoke a named method (re-entering).
fn call_method(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = host
        .as_str(args[1])
        .ok_or_else(|| host.type_error("call_method name must be a string"))?
        .to_owned();
    host.invoke(args[0], &name, &[args[2]])
}

/// `dict_put(d, k, v)` - insert into a dict (re-entering via `__hash__`/
/// `__eq__`) and return the dict. Propagates a throwing `__hash__`.
fn dict_put(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    host.dict_set(args[0], args[1], args[2])?;
    Ok(args[0])
}

/// `set_put(s, item)` - add to a set (re-entering) and return the set.
fn set_put(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    host.set_add(args[0], args[1])?;
    Ok(args[0])
}

// --- rooting + GC --------------------------------------------------------

/// `keep_across(f)` - allocate a string, root it, re-enter by calling `f`
/// (under stress-GC a collection fires here at every instruction), then
/// return the rooted string. Fails loudly if the rooting contract is broken.
fn keep_across(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let kept = host.make_str("kept-alive");
    let guard = host.rooted(kept);
    host.call(args[0], &[])?; // re-enter; GC may sweep everything unrooted
    Ok(guard.get()) // survives because it was rooted
}

/// `big_probe(n)` - return `int_get(n)` as an int when it fits, else fall back
/// to the display string for a big integer. Exercises the bigint-overflow path.
fn big_probe(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    match host.decode(args[0]) {
        ArgValue::Int(n) => Ok(host.make_str(&format!("fits:{n}"))),
        ArgValue::BigInt(_) => Ok(host.make_str(&format!("big:{}", host.display_string(args[0])))),
        _ => Err(host.type_error("big_probe expects an integer")),
    }
}

// --- a plugin class ------------------------------------------------------
//
// `Counter` demonstrates the three ways a plugin class can hold state:
//   1. Hidden native data (`count`) - a plain Rust field, reachable from
//      generic code only through the `value`/`increment` methods.
//   2. A managed `GenericValue` (`label`) - stored in native memory and
//      reported to the collector by `counter_traverse`, so the GC keeps it
//      alive for as long as the instance lives.
//   3. A generic-side attribute (`note`) - an ordinary instance field, set
//      and read from native code via `attr_set`/`attr_get` and equally
//      visible to generic code as `self.note`.

// Methods take the receiver (`self`) as a separate parameter; `args` are the
// remaining arguments only, and arities in `export_module!` exclude the receiver.

/// Per-instance native state, fully hidden from generic code. Allocated on the
/// heap in `counter_init` and freed by `counter_drop`.
struct CounterState {
    /// Pure native data: only observable through `value`/`increment`.
    count: i64,
    /// A held `GenericValue` the GC must be told about (see `counter_traverse`).
    label: GenericValue,
}

/// Destructor for the opaque state; the host calls it when a `Counter` (or a
/// user subclass of it) is garbage-collected.
///
/// # Safety
///
/// Per `ClassDesc::drop`: `ptr` must be null or the pointer installed on a
/// `Counter` instance by `counter_init`, freed exactly once. The host
/// guarantees both.
unsafe extern "C" fn counter_drop(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: by the contract above, `ptr` was produced by `Box::into_raw` in
    // `counter_init` and is being freed for the first time.
    drop(unsafe { Box::from_raw(ptr.cast::<CounterState>()) });
}

/// GC traversal: report the held `label` so it is not swept while the instance
/// is alive. A struct holding no `GenericValue`s would set `traverse: None`.
///
/// # Safety
///
/// Per `PluginTraverseFn`: `ptr` must be null or a `CounterState` installed by
/// `counter_init`, and `visit`/`visit_ctx` the host's marking function and its
/// context, valid for this call.
unsafe extern "C" fn counter_traverse(
    ptr: *mut c_void,
    visit: PluginVisitFn,
    visit_ctx: *mut c_void,
) -> i32 {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: by the contract above, `ptr` points at a live `CounterState`
    // installed by `counter_init`, and `visit`/`visit_ctx` belong together and
    // are live for this call.
    unsafe {
        let state = &*ptr.cast::<CounterState>();
        visit(visit_ctx, state.label);
    }
    0
}

/// Recover the hidden native state, or a `TypeError` if `__init__` never ran.
/// The `unsafe` (asserting the opaque type) lives here once instead of in every
/// method; the receiver is guaranteed to be a `Counter`, so the type holds.
// The `&mut` derives from the opaque pointer, not from `&Host` (interior
// mutability), so a shared borrow only scopes the call.
#[allow(clippy::mut_from_ref)]
fn counter_state<'h>(
    host: &'h Host,
    receiver: GenericValue,
) -> Result<&'h mut CounterState, PluginError> {
    // SAFETY: every `Counter` (and subclass) instance's opaque state is a
    // `CounterState` installed by `counter_init`.
    unsafe { host.opaque_ref::<CounterState>(receiver) }
        .ok_or_else(|| host.type_error("Counter method called on an uninitialized instance"))
}

/// `Counter.__init__(label)` - validate, allocate the hidden state (holding
/// `label`), and record a generic-side `origin` attribute. `this` is the
/// receiver; the label is `args[0]`.
fn counter_init(
    host: &mut Host,
    this: GenericValue,
    args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    // Validate before allocating, so a rejected construction leaves nothing
    // behind (and demonstrates an `__init__` that can fail).
    if matches!(host.decode(args[0]), ArgValue::Nil) {
        return Err(host.type_error("Counter label must not be nil"));
    }
    let state = Box::new(CounterState {
        count: 0,
        label: args[0],
    });
    // SAFETY: a freshly boxed `CounterState`, leaked into the host's keeping -
    // exactly what this class's `counter_drop` frees and `counter_traverse`
    // reads. Nothing else holds it.
    unsafe { host.set_opaque(this, Box::into_raw(state).cast::<c_void>()) }?;
    // A generic-side attribute set from native code; readable as `this.origin`.
    let origin = host.make_str("counter");
    host.attr_set(this, "origin", origin)?;
    // Like every `__init__`, return the receiver: its return value becomes the
    // result of the construction expression.
    Ok(this)
}

/// `Counter.increment()` - bump and return the hidden counter.
fn counter_increment(
    host: &mut Host,
    this: GenericValue,
    _args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    let state = counter_state(host, this)?;
    state.count += 1;
    Ok(host.make_int(state.count))
}

/// `Counter.value()` - read the hidden counter (native data, no generic field).
fn counter_value(
    host: &mut Host,
    this: GenericValue,
    _args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    Ok(host.make_int(counter_state(host, this)?.count))
}

/// `Counter.label()` - return the GC-managed held value. If `counter_traverse`
/// failed to report it, stress-GC would have swept it and this would be garbage.
fn counter_label(
    host: &mut Host,
    this: GenericValue,
    _args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    Ok(counter_state(host, this)?.label)
}

/// `Counter.set_note(note)` - store a generic-side attribute via `attr_set`.
fn counter_set_note(
    host: &mut Host,
    this: GenericValue,
    args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    host.attr_set(this, "note", args[0])?;
    Ok(host.make_nil())
}

/// `Counter.get_note()` - read the generic-side attribute via `attr_get`
/// (`AttributeError` if unset), mirroring generic code reading `this.note`.
fn counter_get_note(
    host: &mut Host,
    this: GenericValue,
    _args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    host.attr_get(this, "note")
}

/// `Counter.__add__(other)` - a dunder implemented by a plugin class, so
/// `a + b` on two counters works and returns a new `Counter` holding the sum.
/// `other` is type-checked against this instance's class before its opaque
/// state is read: `is_instance` first, so reading a foreign instance's opaque
/// pointer as a `CounterState` (a type confusion) can never happen.
fn counter_add(
    host: &mut Host,
    this: GenericValue,
    args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    let other = args[0];
    let class = host.class_of(this)?;
    if !host.is_instance(other, class)? {
        return Err(host.type_error("Counter.__add__ expects another Counter"));
    }
    let a = counter_state(host, this)?.count;
    let b = counter_state(host, other)?.count;
    let label = counter_state(host, this)?.label;
    // Construct a new instance of our own class (the analogue of
    // `type(self)(...)`), then set its hidden count.
    let new = host.call(class, &[label])?;
    counter_state(host, new)?.count = a + b;
    Ok(new)
}

// A second plugin class with its own opaque type, so the tests can check that
// `counter + ticket` is a clean `TypeError` (not a type confusion) rather than
// reading a `TicketState` as a `CounterState`.

struct TicketState;

/// # Safety
///
/// As `counter_drop`, for a `Ticket` instance.
unsafe extern "C" fn ticket_drop(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: by the contract above, `ptr` was produced by `Box::into_raw` in
    // `ticket_init` and is being freed for the first time.
    drop(unsafe { Box::from_raw(ptr.cast::<TicketState>()) });
}

/// `Ticket.__init__()` - a minimal plugin class (distinct opaque type, no held
/// values so no `traverse`).
fn ticket_init(
    host: &mut Host,
    this: GenericValue,
    _args: &[GenericValue],
) -> Result<GenericValue, PluginError> {
    // SAFETY: as in `counter_init`: a fresh box for `ticket_drop` to free.
    unsafe { host.set_opaque(this, Box::into_raw(Box::new(TicketState)).cast::<c_void>()) }?;
    Ok(this)
}

// --- module values --------------------------------------------------------

/// `answer` - a module constant, built once when the module is imported.
fn make_answer(host: &mut Host) -> Result<GenericValue, PluginError> {
    Ok(host.make_int(42))
}

/// `greeting` - a string module constant.
fn make_greeting(host: &mut Host) -> Result<GenericValue, PluginError> {
    Ok(host.make_str("hello from the plugin"))
}

/// `items` - a list module constant: the module binding is a constant, the
/// list itself stays an ordinary mutable list.
fn make_items(host: &mut Host) -> Result<GenericValue, PluginError> {
    let list = host.make_list();
    for n in [1_i64, 2] {
        let item = host.make_int(n);
        host.list_push(list, item)?;
    }
    Ok(list)
}

generic_lang_api::export_module![
    value("answer", make_answer),
    value("greeting", make_greeting),
    value("items", make_items),
    ("add", &[2], add),
    ("shout", &[1], shout),
    ("sum", &[1], sum),
    ("identity", &[1], identity),
    ("kind_name", &[1], kind_name),
    ("raise", &[2], raise),
    ("throw_type", &[1], throw_type),
    ("boom", &[0], boom),
    ("call_with_21_and_double", &[1], call_with_21_and_double),
    ("describe", &[1], describe),
    ("get_field", &[2], get_field),
    ("call_method", &[3], call_method),
    ("dict_put", &[3], dict_put),
    ("set_put", &[2], set_put),
    ("keep_across", &[1], keep_across),
    ("big_probe", &[1], big_probe),
    // Method arities exclude the receiver: `__init__` takes one arg (the label),
    // the readers take none, `__add__` takes one (the other operand).
    class("Counter") {
        ("__init__", &[1], counter_init),
        ("increment", &[0], counter_increment),
        ("value", &[0], counter_value),
        ("label", &[0], counter_label),
        ("set_note", &[1], counter_set_note),
        ("get_note", &[0], counter_get_note),
        ("__add__", &[1], counter_add),
        drop: counter_drop,
        traverse: counter_traverse,
    },
    class("Ticket") {
        ("__init__", &[0], ticket_init),
        drop: ticket_drop,
    },
];

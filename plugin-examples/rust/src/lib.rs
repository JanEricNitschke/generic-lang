//! Rust test-plugin fixture — the primary end-to-end exercise of the generic
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

use generic_lang_api::{ArgValue, GenericValue, Host, PluginError};

// --- happy paths ---------------------------------------------------------

/// `add(a, b)` — numeric addition over the int/float mixes. Demonstrates
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

/// `shout(s)` — uppercase a string and append `!`. Demonstrates borrowing a
/// host string and interning a new one.
fn shout(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let Some(s) = host.as_str(args[0]) else {
        return Err(host.type_error("shout expects a string"));
    };
    let loud = format!("{}!", s.to_uppercase());
    Ok(host.make_str(&loud))
}

/// `sum(list)` — sum a list of numbers. Straight-line code: `list_get` never
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

/// `identity(v)` — return the argument unchanged. Round-trips every value
/// kind through the FFI for the value-integrity tests.
fn identity(_host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    Ok(args[0])
}

/// `kind_name(v)` — the [`ValueKind`](generic_lang_api::ValueKind) of a value
/// as a string. Lets `.gen` tests assert the host's kind classification.
fn kind_name(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = format!("{:?}", host.kind(args[0]));
    Ok(host.make_str(&name))
}

// --- error channels ------------------------------------------------------

/// `raise(class_name, message)` — throw a fresh instance of the named builtin
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

/// `throw_type(message)` — typed-constructor form; caught by `catch TypeError`.
fn throw_type(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let msg = host.as_str(args[0]).unwrap_or("type error").to_owned();
    Err(host.type_error(&msg))
}

/// `boom()` — panic. The `export_module!` glue catches it and turns it into a
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

/// `call_with_21_and_double(f)` — call a generic callable with `21` and double the result.
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

/// `describe(x)` — string form honoring a user class's `__str__`
/// (re-entering). Prefixes `describe:` so tests can see the plugin ran.
fn describe(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let text = host.to_str(args[0])?; // honors __str__; re-enters
    let owned = host.as_str(text).unwrap_or_default().to_owned();
    Ok(host.make_str(&format!("describe:{owned}")))
}

/// `get_field(instance, name)` — read a field via `attr_get` (never re-enters).
fn get_field(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = host
        .as_str(args[1])
        .ok_or_else(|| host.type_error("get_field name must be a string"))?
        .to_owned();
    host.attr_get(args[0], &name)
}

/// `call_method(instance, name, arg)` — invoke a named method (re-entering).
fn call_method(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let name = host
        .as_str(args[1])
        .ok_or_else(|| host.type_error("call_method name must be a string"))?
        .to_owned();
    host.invoke(args[0], &name, &[args[2]])
}

/// `dict_put(d, k, v)` — insert into a dict (re-entering via `__hash__`/
/// `__eq__`) and return the dict. Propagates a throwing `__hash__`.
fn dict_put(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    host.dict_set(args[0], args[1], args[2])?;
    Ok(args[0])
}

/// `set_put(s, item)` — add to a set (re-entering) and return the set.
fn set_put(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    host.set_add(args[0], args[1])?;
    Ok(args[0])
}

// --- rooting + GC --------------------------------------------------------

/// `keep_across(f)` — allocate a string, root it, re-enter by calling `f`
/// (under stress-GC a collection fires here at every instruction), then
/// return the rooted string. Fails loudly if the rooting contract is broken.
fn keep_across(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    let kept = host.make_str("kept-alive");
    let guard = host.rooted(kept);
    host.call(args[0], &[])?; // re-enter; GC may sweep everything unrooted
    Ok(guard.get()) // survives because it was rooted
}

/// `big_probe(n)` — return `int_get(n)` as an int when it fits, else fall back
/// to the display string for a big integer. Exercises the bigint-overflow path.
fn big_probe(host: &mut Host, args: &[GenericValue]) -> Result<GenericValue, PluginError> {
    match host.decode(args[0]) {
        ArgValue::Int(n) => Ok(host.make_str(&format!("fits:{n}"))),
        ArgValue::BigInt(_) => Ok(host.make_str(&format!("big:{}", host.display_string(args[0])))),
        _ => Err(host.type_error("big_probe expects an integer")),
    }
}

generic_lang_api::export_module![
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
];

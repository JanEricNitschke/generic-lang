//! The `time` stdlib module: wall-clock and monotonic time, sleep, and
//! UTC date/time strings.
//!
//! `time` shares the `clock` builtin's implementation (seconds since the
//! Unix epoch). `monotonic` measures from an arbitrary process-fixed
//! origin and never goes backwards; the `_ns` variants return exact
//! integer nanoseconds (the finest unit `std::time` represents).

// Native functions must return `VmResult<Value>` to match the
// calling convention, so infallible ones still wrap in `Ok`.
#![allow(clippy::unnecessary_wraps)]

use std::sync::LazyLock;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use num_bigint::BigInt;

use crate::natives::native_functions::clock_native;
use crate::value::{ModuleContents, ModuleExport, Value};
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// The fixed origin for `monotonic`/`monotonic_ns`.
static ORIGIN: LazyLock<Instant> = LazyLock::new(Instant::now);

/// Nanoseconds as a value: `i64` when they fit, a big int otherwise.
fn nanoseconds_value(vm: &mut VM, nanoseconds: u128) -> Value {
    i64::try_from(nanoseconds).map_or_else(
        |_| vm.heap.add_big_int(BigInt::from(nanoseconds)),
        Value::from,
    )
}

/// `time_ns()` - integer nanoseconds since the Unix epoch.
fn time_ns_native(vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    let Ok(since_epoch) = SystemTime::now().duration_since(UNIX_EPOCH) else {
        return Err(vm
            .throw(ValueError, "system clock is set before the Unix epoch")
            .unwrap_err());
    };
    Ok(nanoseconds_value(vm, since_epoch.as_nanos()))
}

/// `monotonic()` - float seconds since an arbitrary fixed origin;
/// unaffected by wall-clock adjustments and never decreasing.
fn monotonic_native(_vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    Ok(ORIGIN.elapsed().as_secs_f64().into())
}

/// `monotonic_ns()` - integer nanoseconds since the same origin.
fn monotonic_ns_native(vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    let nanoseconds = ORIGIN.elapsed().as_nanos();
    Ok(nanoseconds_value(vm, nanoseconds))
}

/// `sleep(seconds)` - sleep for a non-negative integer or float number
/// of seconds; fractional durations are honored.
fn sleep_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let seconds = match args[0] {
        Value::Number(number) => Some(number.to_f64(&vm.heap)),
        _ => None,
    };
    match seconds {
        Some(seconds) if seconds >= 0.0 && seconds.is_finite() => {
            // Rejects durations beyond what `Duration` represents
            // (u64 seconds) instead of panicking.
            let Ok(duration) = Duration::try_from_secs_f64(seconds) else {
                return Err(vm
                    .throw(
                        ValueError,
                        &format!("'sleep' duration is too large: {seconds}"),
                    )
                    .unwrap_err());
            };
            thread::sleep(duration);
            Ok(Value::Nil)
        }
        _ => Err(vm
            .throw(
                TypeError,
                &format!(
                    "'sleep' expects a non-negative number of seconds, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// Current UTC time split into date and time-of-day parts, ignoring
/// leap seconds (the Unix time convention). `None` when the wall clock is
/// set before the Unix epoch.
fn utc_now() -> Option<(i64, u64)> {
    let since_epoch = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs();
    let days = i64::try_from(since_epoch / 86_400).expect("days fit i64");
    Some((days, since_epoch % 86_400))
}

/// Gregorian civil date from days since the Unix epoch
/// (Howard Hinnant's `civil_from_days`, exact integer arithmetic).
fn civil_from_days(days_since_epoch: i64) -> (i64, u8, u8) {
    let z = days_since_epoch + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let day_of_era = z - era * 146_097;
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let year = year_of_era + era * 400;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_prime = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_prime + 2) / 5 + 1;
    let month = if month_prime < 10 {
        month_prime + 3
    } else {
        month_prime - 9
    };
    let year = if month <= 2 { year + 1 } else { year };
    (
        year,
        u8::try_from(month).expect("month in 1..=12"),
        u8::try_from(day).expect("day in 1..=31"),
    )
}

/// `utc_date()` - the current UTC date as `YYYY-MM-DD`.
fn utc_date_native(vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    let Some((days, _)) = utc_now() else {
        return Err(vm
            .throw(ValueError, "system clock is set before the Unix epoch")
            .unwrap_err());
    };
    let (year, month, day) = civil_from_days(days);
    let date = format!("{year:04}-{month:02}-{day:02}");
    Ok(vm.heap.string_id(&date).into())
}

/// `utc_time()` - the current UTC time of day as `HH:MM:SS` (24h).
fn utc_time_native(vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    let Some((_, seconds_of_day)) = utc_now() else {
        return Err(vm
            .throw(ValueError, "system clock is set before the Unix epoch")
            .unwrap_err());
    };
    let time = format!(
        "{:02}:{:02}:{:02}",
        seconds_of_day / 3600,
        (seconds_of_day / 60) % 60,
        seconds_of_day % 60
    );
    Ok(vm.heap.string_id(&time).into())
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"time", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "time",
            arity: &[0],
            fun: clock_native,
        },
        ModuleExport::Function {
            name: "time_ns",
            arity: &[0],
            fun: time_ns_native,
        },
        ModuleExport::Function {
            name: "monotonic",
            arity: &[0],
            fun: monotonic_native,
        },
        ModuleExport::Function {
            name: "monotonic_ns",
            arity: &[0],
            fun: monotonic_ns_native,
        },
        ModuleExport::Function {
            name: "sleep",
            arity: &[1],
            fun: sleep_native,
        },
        ModuleExport::Function {
            name: "utc_date",
            arity: &[0],
            fun: utc_date_native,
        },
        ModuleExport::Function {
            name: "utc_time",
            arity: &[0],
            fun: utc_time_native,
        },
    ]
}

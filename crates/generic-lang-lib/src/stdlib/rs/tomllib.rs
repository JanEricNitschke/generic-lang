//! The `tomllib` stdlib module: parse TOML documents into generic values.

use std::fs;

use jiff::civil::{Date, DateTime as CivilDateTime, Time};
use jiff::tz::{Offset, TimeZone};

use super::pathlib::as_path_buf;
use super::utils::{type_error, value_error};
use crate::value::{Dict, Instance, ModuleContents, ModuleExport, NativeClass, Value};
use crate::vm::ExceptionKind::IoError;
use crate::vm::VM;
use crate::vm::errors::VmResult;

fn new_instance(vm: &mut VM, class: &str, backing: NativeClass) -> VmResult<Value> {
    let Some(&class) = vm.heap.native_classes.get(class) else {
        return Err(value_error(vm, "date/time module is not available"));
    };
    Ok(vm.heap.add_instance(Instance::new(class, Some(backing))))
}

fn parse_date(vm: &mut VM, date: toml::value::Date) -> VmResult<Date> {
    let year = i16::try_from(date.year)
        .map_err(|_| value_error(vm, "invalid TOML date: year is out of range"))?;
    let month = i8::try_from(date.month)
        .map_err(|_| value_error(vm, "invalid TOML date: month is out of range"))?;
    let day = i8::try_from(date.day)
        .map_err(|_| value_error(vm, "invalid TOML date: day is out of range"))?;
    Date::new(year, month, day).map_err(|_| value_error(vm, "invalid TOML date"))
}

fn parse_time(vm: &mut VM, time: toml::value::Time) -> VmResult<Time> {
    if time.second == Some(60) {
        return Err(value_error(vm, "TOML leap seconds are not supported"));
    }
    let hour = i8::try_from(time.hour)
        .map_err(|_| value_error(vm, "invalid TOML time: hour is out of range"))?;
    let minute = i8::try_from(time.minute)
        .map_err(|_| value_error(vm, "invalid TOML time: minute is out of range"))?;
    let second = i8::try_from(time.second.unwrap_or(0))
        .map_err(|_| value_error(vm, "invalid TOML time: second is out of range"))?;
    // The language exposes microsecond precision; TOML nanoseconds are
    // truncated to the nearest lower microsecond.
    let nanosecond = (time.nanosecond.unwrap_or(0) / 1_000) * 1_000;
    let nanosecond = i32::try_from(nanosecond)
        .map_err(|_| value_error(vm, "invalid TOML time: nanosecond is out of range"))?;
    Time::new(hour, minute, second, nanosecond).map_err(|_| value_error(vm, "invalid TOML time"))
}

fn parse_offset(vm: &mut VM, offset: toml::value::Offset) -> VmResult<Offset> {
    match offset {
        toml::value::Offset::Z => Ok(Offset::UTC),
        toml::value::Offset::Custom { minutes } => {
            let seconds = i32::from(minutes)
                .checked_mul(60)
                .ok_or_else(|| value_error(vm, "invalid TOML offset"))?;
            Offset::from_seconds(seconds).map_err(|_| value_error(vm, "invalid TOML offset"))
        }
    }
}

fn to_generic_datetime(vm: &mut VM, datetime: toml::value::Datetime) -> VmResult<Value> {
    match (datetime.date, datetime.time, datetime.offset) {
        (Some(date), None, None) => {
            let date = parse_date(vm, date)?;
            new_instance(vm, "Date", NativeClass::Date(date))
        }
        (None, Some(time), None) => {
            let time = parse_time(vm, time)?;
            new_instance(vm, "Time", NativeClass::Time(time))
        }
        (Some(date), Some(time), Some(offset)) => {
            let date = parse_date(vm, date)?;
            let time = parse_time(vm, time)?;
            let offset = parse_offset(vm, offset)?;
            let civil = CivilDateTime::from_parts(date, time);
            let datetime = TimeZone::fixed(offset)
                .to_zoned_datetime(civil)
                .map_err(|error| value_error(vm, &format!("invalid TOML date-time: {error}")))?;
            new_instance(vm, "DateTime", NativeClass::DateTime(datetime))
        }
        (Some(_), Some(_), None) => Err(value_error(vm, "TOML local date-times are not supported")),
        _ => Err(value_error(vm, "invalid TOML date-time")),
    }
}

fn to_generic(vm: &mut VM, value: &toml::Value) -> VmResult<Value> {
    Ok(match value {
        toml::Value::String(string) => vm.heap.string_id(string).into(),
        toml::Value::Integer(integer) => (*integer).into(),
        toml::Value::Float(float) => (*float).into(),
        toml::Value::Boolean(boolean) => (*boolean).into(),
        toml::Value::Datetime(datetime) => to_generic_datetime(vm, *datetime)?,
        toml::Value::Array(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(to_generic(vm, item)?);
            }
            vm.new_list(values)
        }
        toml::Value::Table(table) => {
            // Keys are strings, so Dict::add only hashes/compares native
            // strings; no user code can re-enter and no extra rooting is needed.
            let dict = vm.new_dict();
            for (key, value) in table {
                let key = vm.heap.string_id(key).into();
                let value = to_generic(vm, value)?;
                Dict::add(vm, &dict, key, value)?;
            }
            dict
        }
    })
}

fn parse_toml_source(vm: &mut VM, source: &str) -> VmResult<Value> {
    match toml::from_str::<toml::Value>(source) {
        Ok(toml) => to_generic(vm, &toml),
        Err(error) => Err(value_error(
            vm,
            &format!("Invalid TOML: {}", error.to_string().trim()),
        )),
    }
}

fn loads_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::String(source) = args[0] else {
        return Err(type_error(vm, "loads", "a string", args[0]));
    };
    let source = source.to_value(&vm.heap).clone();
    parse_toml_source(vm, &source)
}

fn load_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let path = as_path_buf(vm, args[0], "'load'")?;
    let source = fs::read_to_string(&path).map_err(|error| {
        vm.throw(
            IoError,
            &format!("Could not read `{}`: {error}", path.display()),
        )
        .unwrap_err()
    })?;
    parse_toml_source(vm, &source)
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"tomllib", module());
}

fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "loads",
            arity: &[1],
            fun: loads_native,
        },
        ModuleExport::Function {
            name: "load",
            arity: &[1],
            fun: load_native,
        },
    ]
}

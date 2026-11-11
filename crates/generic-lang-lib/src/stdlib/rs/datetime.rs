//! The `datetime` stdlib module: calendar dates, wall-clock times, zoned
//! date-times, durations, and fixed or IANA time zones.

// Native functions must return `VmResult<Value>`, so infallible ones still
// wrap in `Ok`.
#![allow(clippy::unnecessary_wraps)]

use std::hash::{Hash, Hasher};

use jiff::civil::{Date, DateTime as CivilDateTime, Time};
use jiff::fmt::temporal::DateTimeParser;
use jiff::tz::{Offset, TimeZone};
use jiff::{SignedDuration, ZonedDateTime};
use rustc_hash::FxHasher;

use crate::value::{
    Instance, ModuleContents, ModuleExport, NativeClass, Number, Timedelta, Timezone, Value,
};
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::{VmErrorKind, VmResult};

fn type_error(vm: &mut VM, what: &str, expected: &str, value: Value) -> VmErrorKind {
    vm.throw(
        TypeError,
        &format!(
            "'{what}' expects {expected}, got: {}",
            value.to_string(&vm.heap)
        ),
    )
    .unwrap_err()
}

fn value_error(vm: &mut VM, message: &str) -> VmErrorKind {
    vm.throw(ValueError, message).unwrap_err()
}

fn int_arg(vm: &mut VM, value: Value, what: &str) -> VmResult<i64> {
    let Value::Number(Number::Integer(integer)) = value else {
        return Err(type_error(vm, what, "an integer", value));
    };
    integer
        .try_to_i64(&vm.heap)
        .map_err(|_| value_error(vm, &format!("'{what}' integer is out of range")))
}

fn i8_arg(vm: &mut VM, value: Value, what: &str) -> VmResult<i8> {
    let value = int_arg(vm, value, what)?;
    i8::try_from(value).map_err(|_| value_error(vm, &format!("'{what}' is out of range")))
}

fn i16_arg(vm: &mut VM, value: Value, what: &str) -> VmResult<i16> {
    let value = int_arg(vm, value, what)?;
    i16::try_from(value).map_err(|_| value_error(vm, &format!("'{what}' is out of range")))
}

fn u32_arg(vm: &mut VM, value: Value, what: &str) -> VmResult<u32> {
    let value = int_arg(vm, value, what)?;
    u32::try_from(value).map_err(|_| value_error(vm, &format!("'{what}' is out of range")))
}

fn month_arg(vm: &mut VM, value: Value) -> VmResult<i8> {
    let month = i8_arg(vm, value, "month")?;
    if (1..=12).contains(&month) {
        Ok(month)
    } else {
        Err(value_error(vm, "'month' is out of range"))
    }
}

/// Parse a microsecond argument and convert it to the nanoseconds Jiff uses.
fn microsecond_nanoseconds(vm: &mut VM, value: Value) -> VmResult<i32> {
    let microsecond = u32_arg(vm, value, "microsecond")?;
    if microsecond > 999_999 {
        return Err(value_error(vm, "'microsecond' is out of range"));
    }
    Ok(i32::try_from(microsecond).expect("microsecond fits i32") * 1_000)
}

/// Convert whole minutes to a Jiff offset, keeping the input and its
/// equivalent seconds in any range error.
fn offset_from_minutes(vm: &mut VM, minutes: i64) -> VmResult<Offset> {
    let seconds = i32::try_from(minutes)
        .ok()
        .and_then(|minutes| minutes.checked_mul(60))
        .ok_or_else(|| value_error(vm, "'offset' is out of range"))?;
    Offset::from_seconds(seconds).map_err(|error| {
        value_error(
            vm,
            &format!("invalid offset: {minutes} minutes ({seconds} seconds): {error}"),
        )
    })
}

fn timedelta_of(vm: &mut VM, value: Value, what: &str) -> VmResult<SignedDuration> {
    if matches!(value, Value::Instance(instance) if matches!(instance.to_value(&vm.heap).backing, Some(NativeClass::Timedelta(_))))
    {
        return Ok(value.as_timedelta(&vm.heap).0);
    }
    Err(type_error(vm, what, "a Timedelta", value))
}

fn timezone_of(vm: &mut VM, value: Value, what: &str) -> VmResult<TimeZone> {
    if matches!(value, Value::Instance(instance) if matches!(instance.to_value(&vm.heap).backing, Some(NativeClass::Timezone(_))))
    {
        return Ok(value.as_timezone(&vm.heap).0.clone());
    }
    Err(type_error(vm, what, "a Timezone", value))
}

fn set_backing(vm: &mut VM, receiver: &Value, backing: NativeClass) {
    let instance = *receiver.as_instance();
    instance.to_value_mut(&mut vm.heap).backing = Some(backing);
}

fn new_instance(vm: &mut VM, class: &str, backing: NativeClass) -> Value {
    let class = *vm
        .heap
        .native_classes
        .get(class)
        .expect("datetime class is registered");
    vm.heap.add_instance(Instance::new(class, Some(backing)))
}

fn string_arg(vm: &mut VM, value: Value, what: &str) -> VmResult<String> {
    let Value::String(string) = value else {
        return Err(type_error(vm, what, "a string", value));
    };
    Ok(string.to_value(&vm.heap).clone())
}

/// Parse an ISO 8601 datetime with either a bracketed zone or numeric offset.
///
/// Jiff's `ZonedDateTime` parser requires a bracket annotation, so offset-only
/// strings fall back to `Pieces`.
fn parse_datetime(source: &str) -> Result<ZonedDateTime, String> {
    const EXPECTED: &str =
        "invalid datetime: expected an ISO 8601 datetime with a UTC offset or time zone";

    let parser = DateTimeParser::new();
    if let Ok(datetime) = parser.parse_zoned_datetime(source) {
        if datetime.time_zone().is_unknown() {
            // Strict parse succeeded, but Jiff recognized an unknown zone:
            //   "2020-01-02T12:00:00Z[Etc/Unknown]"
            return Err("invalid datetime: unknown time zone".to_string());
        }
        // Strict parse succeeded for a known zone:
        //   "2020-01-02T12:00:00+01:00[Europe/Berlin]"
        return Ok(datetime);
    }

    // Both parsers reject malformed input:
    //   "not-a-datetime"
    let pieces = parser
        .parse_pieces(source)
        .map_err(|_| EXPECTED.to_string())?;

    let time = pieces.time().unwrap_or_else(Time::midnight);
    let civil = pieces.date().to_datetime(time);

    if pieces.time_zone_annotation().is_some() {
        // The annotation is present, but its zone lookup failed:
        //   "2020-01-02T12:00:00+00:00[Not/AZone]"
        let timezone = pieces
            .to_time_zone()
            .map_err(|_| "invalid datetime: unknown time zone".to_string())?
            .ok_or_else(|| "invalid datetime: unknown time zone".to_string())?;
        if timezone.is_unknown() {
            // The annotation names Jiff's reserved unknown zone:
            //   "2020-01-02T12:00:00+01:00[Etc/Unknown]"
            return Err("invalid datetime: unknown time zone".to_string());
        }

        let resolved = match timezone.to_zoned_datetime(civil) {
            Ok(resolved) => resolved,
            Err(error) => {
                // The zone resolves, but the datetime is outside Jiff's range:
                //   "9999-12-31T23:59:59+00:00[UTC]"
                return Err(format!("invalid datetime: {error}"));
            }
        };

        if resolved.datetime() != civil {
            // The local time does not exist because of a DST gap, for example:
            //   "2024-03-10T02:30:00-04:00[America/New_York]"
            return Err("invalid datetime: local time does not exist in the time zone".to_string());
        }

        if let Some(offset) = pieces.to_numeric_offset()
            && offset != resolved.offset()
        {
            // The zone resolves, but the supplied offset disagrees with it:
            //   "2024-01-01T12:00:00+02:00[Europe/Paris]"
            return Err("invalid datetime: offset does not match the time zone".to_string());
        }

        // The zone and offset agree:
        //   "2024-11-03T01:30:00-05:00[America/New_York]"
        return Ok(resolved);
    }

    // There is no annotation and no numeric offset:
    //   "2020-01-02T12:00:00"
    let offset = pieces
        .to_numeric_offset()
        .ok_or_else(|| EXPECTED.to_string())?;

    match TimeZone::fixed(offset).to_zoned_datetime(civil) {
        // Offset-only input resolves successfully:
        //   "2020-01-02T12:00:00+02:00"
        Ok(datetime) => Ok(datetime),
        Err(error) => {
            // Offset-only input is outside Jiff's range:
            //   "9999-12-31T23:59:59+00:00"
            Err(format!("invalid datetime: {error}"))
        }
    }
}

fn date_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let date = if args.len() == 1 {
        let source = string_arg(vm, args[0], "Date")?;
        source
            .parse::<Date>()
            .map_err(|_| value_error(vm, "invalid date: expected an ISO 8601 date"))?
    } else {
        let year = i16_arg(vm, args[0], "year")?;
        let month = month_arg(vm, args[1])?;
        let day = i8_arg(vm, args[2], "day")?;
        Date::new(year, month, day)
            .map_err(|error| value_error(vm, &format!("invalid date: {error}")))?
    };
    set_backing(vm, receiver, NativeClass::Date(date));
    Ok(*receiver)
}

fn date_year(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date(&vm.heap).year().into())
}

fn date_month(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date(&vm.heap).month().into())
}

fn date_day(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date(&vm.heap).day().into())
}

/// Add a duration to a date, ignoring any sub-day part. `Date` arithmetic
/// is day-granular; use a `DateTime` for second-level arithmetic.
fn date_add_duration(vm: &mut VM, date: Date, duration: SignedDuration) -> VmResult<Date> {
    let days = duration.as_secs() / 86_400;
    let seconds = days
        .checked_mul(86_400)
        .ok_or_else(|| value_error(vm, "date arithmetic is out of range"))?;
    date.checked_add(SignedDuration::from_secs(seconds))
        .map_err(|_| value_error(vm, "date arithmetic is out of range"))
}

fn date_add(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let date = *receiver.as_date(&vm.heap);
    let duration = timedelta_of(vm, args[0], "add")?;
    let date = date_add_duration(vm, date, duration)?;
    Ok(new_instance(vm, "Date", NativeClass::Date(date)))
}

fn date_sub(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let date = *receiver.as_date(&vm.heap);
    let duration = timedelta_of(vm, args[0], "sub")?;
    let duration = duration
        .checked_neg()
        .ok_or_else(|| value_error(vm, "date arithmetic is out of range"))?;
    let date = date_add_duration(vm, date, duration)?;
    Ok(new_instance(vm, "Date", NativeClass::Date(date)))
}

/// Parse a bare ISO 8601 time. `Pieces` needs a date component, so wrap the
/// input in one and reject any parsed offset or zone annotation.
fn parse_time(source: &str) -> Option<Time> {
    let parser = DateTimeParser::new();
    let wrapped = format!("2000-01-01T{source}");
    let pieces = parser.parse_pieces(&wrapped).ok()?;
    if pieces.offset().is_some() || pieces.time_zone_annotation().is_some() {
        return None;
    }
    pieces.time()
}

fn time_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let time = if args.len() == 1 {
        let source = string_arg(vm, args[0], "Time")?;
        parse_time(&source)
            .ok_or_else(|| value_error(vm, "invalid time: expected an ISO 8601 time"))?
    } else {
        let hour = i8_arg(vm, args[0], "hour")?;
        let minute = i8_arg(vm, args[1], "minute")?;
        let second = i8_arg(vm, args[2], "second")?;
        let nanosecond = match args.get(3) {
            Some(value) => microsecond_nanoseconds(vm, *value)?,
            None => 0,
        };
        Time::new(hour, minute, second, nanosecond)
            .map_err(|error| value_error(vm, &format!("invalid time: {error}")))?
    };
    set_backing(vm, receiver, NativeClass::Time(time));
    Ok(*receiver)
}

fn time_hour(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_time(&vm.heap).hour().into())
}

fn time_minute(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_time(&vm.heap).minute().into())
}

fn time_second(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_time(&vm.heap).second().into())
}

fn time_microsecond(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok((receiver.as_time(&vm.heap).subsec_nanosecond() / 1_000).into())
}

fn datetime_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let datetime = if args.len() == 1 {
        let source = string_arg(vm, args[0], "DateTime")?;
        parse_datetime(&source).map_err(|message| value_error(vm, &message))?
    } else {
        let year = i16_arg(vm, args[0], "year")?;
        let month = month_arg(vm, args[1])?;
        let day = i8_arg(vm, args[2], "day")?;
        let hour = i8_arg(vm, args[3], "hour")?;
        let minute = i8_arg(vm, args[4], "minute")?;
        let second = i8_arg(vm, args[5], "second")?;
        let (nanosecond, timezone) = match &args[6..] {
            // 7 args: year, month, day, hour, minute, second, timezone
            [timezone] => (0, timezone_of(vm, *timezone, "timezone")?),
            // 8 args: year, month, day, hour, minute, second, microsecond, timezone
            [microsecond, timezone] => (
                microsecond_nanoseconds(vm, *microsecond)?,
                timezone_of(vm, *timezone, "timezone")?,
            ),
            _ => unreachable!("arity is validated before the native runs"),
        };
        let date = Date::new(year, month, day)
            .map_err(|error| value_error(vm, &format!("invalid date: {error}")))?;
        let time = Time::new(hour, minute, second, nanosecond)
            .map_err(|error| value_error(vm, &format!("invalid time: {error}")))?;
        let civil = CivilDateTime::from_parts(date, time);
        timezone
            .to_zoned_datetime(civil)
            .map_err(|error| value_error(vm, &format!("invalid datetime: {error}")))?
    };
    set_backing(vm, receiver, NativeClass::DateTime(datetime));
    Ok(*receiver)
}

fn datetime_year(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).year().into())
}

fn datetime_month(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).month().into())
}

fn datetime_day(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).day().into())
}

fn datetime_hour(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).hour().into())
}

fn datetime_minute(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).minute().into())
}

fn datetime_second(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver.as_date_time(&vm.heap).second().into())
}

fn datetime_microsecond(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok((receiver.as_date_time(&vm.heap).subsec_nanosecond() / 1_000).into())
}

fn datetime_date(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let date = receiver.as_date_time(&vm.heap).date();
    Ok(new_instance(vm, "Date", NativeClass::Date(date)))
}

fn datetime_time(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let time = receiver.as_date_time(&vm.heap).time();
    Ok(new_instance(vm, "Time", NativeClass::Time(time)))
}

fn datetime_offset(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let offset = receiver.as_date_time(&vm.heap).offset();
    Ok(new_instance(vm, "Offset", NativeClass::Offset(offset)))
}

fn datetime_timezone(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let timezone = receiver.as_date_time(&vm.heap).time_zone().clone();
    Ok(new_instance(
        vm,
        "Timezone",
        NativeClass::Timezone(Timezone(timezone)),
    ))
}

fn datetime_add(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let datetime = receiver.as_date_time(&vm.heap).clone();
    let duration = timedelta_of(vm, args[0], "add")?;
    let result = datetime
        .checked_add(duration)
        .map_err(|_| value_error(vm, "datetime arithmetic is out of range"))?;
    Ok(new_instance(vm, "DateTime", NativeClass::DateTime(result)))
}

fn datetime_sub(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let datetime = receiver.as_date_time(&vm.heap).clone();
    let duration = timedelta_of(vm, args[0], "sub")?;
    let result = datetime
        .checked_sub(duration)
        .map_err(|_| value_error(vm, "datetime arithmetic is out of range"))?;
    Ok(new_instance(vm, "DateTime", NativeClass::DateTime(result)))
}

fn timedelta_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let duration = match args[0] {
        Value::Number(Number::Integer(integer)) => {
            let seconds = integer
                .try_to_i64(&vm.heap)
                .map_err(|_| value_error(vm, "timedelta is out of range"))?;
            SignedDuration::from_secs(seconds)
        }
        Value::Number(number) => {
            let seconds = number.to_f64(&vm.heap);
            if !seconds.is_finite() {
                return Err(value_error(vm, "'seconds' must be finite"));
            }
            SignedDuration::try_from_secs_f64(seconds)
                .map_err(|_| value_error(vm, "timedelta is out of range"))?
        }
        _ => return Err(type_error(vm, "seconds", "a number", args[0])),
    };
    set_backing(vm, receiver, NativeClass::Timedelta(Timedelta(duration)));
    Ok(*receiver)
}

fn timedelta_total_seconds(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(timedelta_of(vm, *receiver, "Timedelta")?
        .as_secs_f64()
        .into())
}

fn timedelta_days(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok((timedelta_of(vm, *receiver, "Timedelta")?.as_secs() / 86_400).into())
}

fn timedelta_seconds(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(timedelta_of(vm, *receiver, "Timedelta")?.as_secs().into())
}

fn timedelta_microseconds(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(timedelta_of(vm, *receiver, "Timedelta")?
        .subsec_micros()
        .into())
}

fn timedelta_add(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let result = timedelta_of(vm, *receiver, "Timedelta")?
        .checked_add(timedelta_of(vm, args[0], "add")?)
        .ok_or_else(|| value_error(vm, "timedelta arithmetic is out of range"))?;
    Ok(new_instance(
        vm,
        "Timedelta",
        NativeClass::Timedelta(Timedelta(result)),
    ))
}

fn timedelta_sub(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let result = timedelta_of(vm, *receiver, "Timedelta")?
        .checked_sub(timedelta_of(vm, args[0], "sub")?)
        .ok_or_else(|| value_error(vm, "timedelta arithmetic is out of range"))?;
    Ok(new_instance(
        vm,
        "Timedelta",
        NativeClass::Timedelta(Timedelta(result)),
    ))
}

fn timezone_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let timezone = match args[0] {
        Value::Number(Number::Integer(_)) => {
            let minutes = int_arg(vm, args[0], "offset")?;
            TimeZone::fixed(offset_from_minutes(vm, minutes)?)
        }
        Value::String(_) => {
            let name = string_arg(vm, args[0], "Timezone")?;
            let timezone = TimeZone::get(&name)
                .map_err(|error| value_error(vm, &format!("invalid timezone: {error}")))?;
            if timezone.is_unknown() {
                return Err(value_error(vm, "invalid timezone: unknown time zone"));
            }
            timezone
        }
        value => {
            return Err(type_error(
                vm,
                "Timezone",
                "an integer offset in minutes or an IANA time zone name",
                value,
            ));
        }
    };
    set_backing(vm, receiver, NativeClass::Timezone(Timezone(timezone)));
    Ok(*receiver)
}

fn fixed_offset_of(vm: &mut VM, receiver: &Value) -> VmResult<Offset> {
    receiver
        .as_timezone(&vm.heap)
        .0
        .to_fixed_offset()
        .map_err(|_| {
            let name = receiver
                .as_timezone(&vm.heap)
                .0
                .iana_name()
                // Unreachable: unknown zones are rejected, and every other
                // non-fixed zone has an IANA name.
                .map_or_else(|| "this timezone".to_string(), |name| format!("'{name}'"));
            value_error(vm, &format!("{name} does not have a fixed offset"))
        })
}

fn timezone_minutes(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let offset = fixed_offset_of(vm, receiver)?;
    if offset.seconds() % 60 != 0 {
        return Err(value_error(
            vm,
            "timezone offset is not a whole number of minutes",
        ));
    }
    Ok((offset.seconds() / 60).into())
}

fn timezone_seconds(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(fixed_offset_of(vm, receiver)?.seconds().into())
}

fn timezone_is_fixed(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(receiver
        .as_timezone(&vm.heap)
        .0
        .to_fixed_offset()
        .is_ok()
        .into())
}

fn timezone_name(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let name = receiver
        .as_timezone(&vm.heap)
        .0
        .iana_name()
        .map(str::to_string);
    Ok(name.map_or(Value::Nil, |name| vm.heap.string_id(&name).into()))
}

fn offset_init(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let minutes = int_arg(vm, args[0], "offset")?;
    let offset = offset_from_minutes(vm, minutes)?;
    set_backing(vm, receiver, NativeClass::Offset(offset));
    Ok(*receiver)
}

fn offset_minutes(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let offset = *receiver.as_offset(&vm.heap);
    if offset.seconds() % 60 != 0 {
        return Err(value_error(vm, "offset is not a whole number of minutes"));
    }
    Ok((offset.seconds() / 60).into())
}

fn offset_seconds(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok((*receiver.as_offset(&vm.heap)).seconds().into())
}

fn offset_timezone(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let offset = *receiver.as_offset(&vm.heap);
    Ok(new_instance(
        vm,
        "Timezone",
        NativeClass::Timezone(Timezone(TimeZone::fixed(offset))),
    ))
}

fn offset_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(hash_parts((*receiver.as_offset(&vm.heap)).seconds()).into())
}

fn timezone_offset(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let offset = fixed_offset_of(vm, receiver)?;
    Ok(new_instance(vm, "Offset", NativeClass::Offset(offset)))
}

fn hash_parts<T: Hash>(parts: T) -> i64 {
    let mut hasher = FxHasher::default();
    parts.hash(&mut hasher);
    hasher.finish().cast_signed()
}

fn date_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(hash_parts(*receiver.as_date(&vm.heap)).into())
}

fn time_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(hash_parts(*receiver.as_time(&vm.heap)).into())
}

fn datetime_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(hash_parts(receiver.as_date_time(&vm.heap).timestamp()).into())
}

fn timedelta_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    Ok(hash_parts(receiver.as_timedelta(&vm.heap).0).into())
}

fn timezone_hash(vm: &mut VM, receiver: &Value, _args: &[Value]) -> VmResult<Value> {
    let timezone = &receiver.as_timezone(&vm.heap).0;
    let hash = match timezone.to_fixed_offset() {
        Ok(offset) => hash_parts(offset.seconds()),
        Err(_) => hash_parts(timezone.primary_iana_name()),
    };
    Ok(hash.into())
}

pub(super) fn register(vm: &mut VM) {
    for name in [
        "Date",
        "Time",
        "DateTime",
        "Timedelta",
        "Timezone",
        "Offset",
    ] {
        vm.define_native_class(&name, false);
    }

    vm.define_native_method(&"Date", &"__init__", &[1, 3], date_init);
    vm.define_native_method(&"Date", &"year", &[0], date_year);
    vm.define_native_method(&"Date", &"month", &[0], date_month);
    vm.define_native_method(&"Date", &"day", &[0], date_day);
    vm.define_native_method(&"Date", &"__hash__", &[0], date_hash);
    vm.define_native_method(&"Date", &"__add__", &[1], date_add);
    vm.define_native_method(&"Date", &"__sub__", &[1], date_sub);

    vm.define_native_method(&"Time", &"__init__", &[1, 3, 4], time_init);
    vm.define_native_method(&"Time", &"hour", &[0], time_hour);
    vm.define_native_method(&"Time", &"minute", &[0], time_minute);
    vm.define_native_method(&"Time", &"second", &[0], time_second);
    vm.define_native_method(&"Time", &"microsecond", &[0], time_microsecond);
    vm.define_native_method(&"Time", &"__hash__", &[0], time_hash);

    vm.define_native_method(&"DateTime", &"__init__", &[1, 7, 8], datetime_init);
    vm.define_native_method(&"DateTime", &"year", &[0], datetime_year);
    vm.define_native_method(&"DateTime", &"month", &[0], datetime_month);
    vm.define_native_method(&"DateTime", &"day", &[0], datetime_day);
    vm.define_native_method(&"DateTime", &"hour", &[0], datetime_hour);
    vm.define_native_method(&"DateTime", &"minute", &[0], datetime_minute);
    vm.define_native_method(&"DateTime", &"second", &[0], datetime_second);
    vm.define_native_method(&"DateTime", &"microsecond", &[0], datetime_microsecond);
    vm.define_native_method(&"DateTime", &"date", &[0], datetime_date);
    vm.define_native_method(&"DateTime", &"time", &[0], datetime_time);
    vm.define_native_method(&"DateTime", &"offset", &[0], datetime_offset);
    vm.define_native_method(&"DateTime", &"timezone", &[0], datetime_timezone);
    vm.define_native_method(&"DateTime", &"__hash__", &[0], datetime_hash);
    vm.define_native_method(&"DateTime", &"__add__", &[1], datetime_add);
    vm.define_native_method(&"DateTime", &"__sub__", &[1], datetime_sub);

    vm.define_native_method(&"Timedelta", &"__init__", &[1], timedelta_init);
    vm.define_native_method(
        &"Timedelta",
        &"total_seconds",
        &[0],
        timedelta_total_seconds,
    );
    vm.define_native_method(&"Timedelta", &"days", &[0], timedelta_days);
    vm.define_native_method(&"Timedelta", &"seconds", &[0], timedelta_seconds);
    vm.define_native_method(&"Timedelta", &"microseconds", &[0], timedelta_microseconds);
    vm.define_native_method(&"Timedelta", &"__add__", &[1], timedelta_add);
    vm.define_native_method(&"Timedelta", &"__sub__", &[1], timedelta_sub);
    vm.define_native_method(&"Timedelta", &"__hash__", &[0], timedelta_hash);

    vm.define_native_method(&"Timezone", &"__init__", &[1], timezone_init);
    vm.define_native_method(&"Timezone", &"minutes", &[0], timezone_minutes);
    vm.define_native_method(&"Timezone", &"seconds", &[0], timezone_seconds);
    vm.define_native_method(&"Timezone", &"name", &[0], timezone_name);
    vm.define_native_method(&"Timezone", &"is_fixed", &[0], timezone_is_fixed);
    vm.define_native_method(&"Timezone", &"offset", &[0], timezone_offset);
    vm.define_native_method(&"Timezone", &"__hash__", &[0], timezone_hash);

    vm.define_native_method(&"Offset", &"__init__", &[1], offset_init);
    vm.define_native_method(&"Offset", &"minutes", &[0], offset_minutes);
    vm.define_native_method(&"Offset", &"seconds", &[0], offset_seconds);
    vm.define_native_method(&"Offset", &"timezone", &[0], offset_timezone);
    vm.define_native_method(&"Offset", &"__hash__", &[0], offset_hash);

    vm.register_stdlib_module(&"datetime", module());
}

fn module() -> ModuleContents {
    vec![
        ModuleExport::Class { name: "Date" },
        ModuleExport::Class { name: "Time" },
        ModuleExport::Class { name: "DateTime" },
        ModuleExport::Class { name: "Timedelta" },
        ModuleExport::Class { name: "Timezone" },
        ModuleExport::Class { name: "Offset" },
    ]
}

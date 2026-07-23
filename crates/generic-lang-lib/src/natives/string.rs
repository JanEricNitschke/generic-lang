use crate::value::{GenericInt, Instance, List, NativeClass, Number, Value};
use crate::vm::ExceptionKind::{IndexError, TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::VmResult;
use unicode_segmentation::UnicodeSegmentation;

/// String.__init__(value) - Convert any value to string using `to_string_native` logic
pub(super) fn string_init_native(
    vm: &mut VM,
    _receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    if let Value::Instance(instance) = &args[0]
        && let Some(NativeClass::List(list)) = &instance.to_value(&vm.heap).backing
        && !list.items.is_empty()
        && let Some(bytes) = list
            .items
            .iter()
            .map(|v| {
                if let Value::Number(Number::Integer(GenericInt::Small(x))) = v {
                    u8::try_from(*x).ok()
                } else {
                    None
                }
            })
            .collect::<Option<Vec<u8>>>()
        && let Ok(string) = String::from_utf8(bytes)
    {
        Ok(Value::String(vm.heap.string_id(&string)))
    } else {
        Ok(Value::String(vm.value_to_string(&args[0])?))
    }
}

// String.replace(old, new) - normalized
pub(super) fn string_replace_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    match args {
        [Value::String(old), Value::String(new)] => {
            let old = old.as_normalized_string(&vm.heap);
            let new = new.as_normalized_string(&vm.heap);

            Ok(Value::String(vm.heap.string_id(&s.replace(&old, &new))))
        }
        _ => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected two strings, got {} and {}",
                    args[0].to_string(&vm.heap),
                    args[1].to_string(&vm.heap),
                ),
            )
            .unwrap_err()),
    }
}

/// String.contains(substring) - normalized
pub(super) fn string_contains_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    if let Value::String(sub) = args[0] {
        let sub = sub.as_normalized_string(&vm.heap);
        Ok(Value::Bool(s.contains(&sub)))
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected string argument, got {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

/// String.find(substring) - normalized
pub(super) fn string_find_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    if let Value::String(sub) = args[0] {
        let sub = sub.as_normalized_string(&vm.heap);
        Ok(Value::Number(Number::Integer(
            s.find(&sub).map_or(GenericInt::Small(-1), |u| {
                GenericInt::from_usize(u, &mut vm.heap)
            }),
        )))
    } else {
        Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected string argument, got {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err())
    }
}

/// Shared utility to convert an iterator of items into a List instance
fn vec_to_list_instance(vm: &mut VM, items: Vec<Value>) -> Value {
    let target_class = vm.heap.native_classes.get("List").unwrap();
    let instance = Instance::new(*target_class, Some(List::new(items).into()));
    vm.heap.add_instance(instance)
}

pub(super) fn string_bytes_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().to_value(&vm.heap);
    let items = s
        .bytes()
        .map(|b| Value::Number(Number::Integer(GenericInt::Small(b.into()))))
        .collect();
    Ok(vec_to_list_instance(vm, items))
}

pub(super) fn string_get_byte_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into bytes with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Can only index into bytes with integer, got `{}`.",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap);
    match s.as_bytes().get(index) {
        Some(b) => Ok(Value::Number(Number::Integer(GenericInt::Small(
            (*b).into(),
        )))),
        None => Err(vm
            .throw(IndexError, "byte index out of bounds")
            .unwrap_err()),
    }
}

pub(super) fn string_chars_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().to_value(&vm.heap).clone();
    let items = s
        .chars()
        .map(|c| Value::String(vm.heap.string_id(&c.to_string())))
        .collect();
    Ok(vec_to_list_instance(vm, items))
}

pub(super) fn string_get_char_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into chars with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Can only index into chars with integer, got `{}`.",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap);
    match s.chars().nth(index) {
        Some(c) => Ok(Value::String(vm.heap.string_id(&c.to_string()))),
        None => Err(vm
            .throw(IndexError, "char index out of bounds")
            .unwrap_err()),
    }
}

pub(super) fn string_clusters_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().to_value(&vm.heap).clone();
    let items = s
        .graphemes(true)
        .map(|g| Value::String(vm.heap.string_id(&g)))
        .collect();
    Ok(vec_to_list_instance(vm, items))
}

pub(super) fn string_get_cluster_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw(ValueError, &format!(
                        "Can not index into clusters with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw(
                    TypeError,
                    &format!(
                        "Can only index into clusters with integer, got `{}`.",
                        x.to_string(&vm.heap)
                    ),
                )
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap).clone();
    match s.graphemes(true).nth(index) {
        Some(cluster) => Ok(Value::String(vm.heap.string_id(&cluster))),
        None => Err(vm
            .throw(IndexError, "cluster index out of bounds")
            .unwrap_err()),
    }
}

/// `String.split([sep])` - split into a list of substrings.
///
/// Without an argument, splits on runs of whitespace and drops empty
/// leading/trailing fields. With a string argument, splits on every occurrence
/// of `sep`, keeping empty fields.
pub(super) fn string_split_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);

    let items = if args.is_empty() {
        s.split_whitespace()
            .map(|part| vm.heap.string_id(&part).into())
            .collect()
    } else {
        match &args[0] {
            Value::String(sep_id) => {
                let sep = sep_id.as_normalized_string(&vm.heap);
                if sep.is_empty() {
                    return Err(vm
                        .throw(ValueError, "Cannot split on an empty separator.")
                        .unwrap_err());
                }
                s.split(&sep)
                    .map(|part| vm.heap.string_id(&part).into())
                    .collect()
            }
            x => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!(
                            "Expected a string separator, got `{}`.",
                            x.to_string(&vm.heap)
                        ),
                    )
                    .unwrap_err());
            }
        }
    };

    Ok(vec_to_list_instance(vm, items))
}

/// `String.join(iterable)` - join string elements with self as separator.
///
/// All elements must be strings. Returns a new string.
pub(super) fn string_join_native(vm: &mut VM, receiver: &Value, args: &[Value]) -> VmResult<Value> {
    let sep = receiver.as_string().as_normalized_string(&vm.heap);

    let Some(items) = vm.collect_items_from_iterable(args[0])? else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected an iterable, got `{}`.",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };

    let mut parts = Vec::with_capacity(items.len());
    for item in &items {
        match item {
            Value::String(id) => {
                parts.push(id.as_normalized_string(&vm.heap));
            }
            _ => {
                return Err(vm
                    .throw(
                        TypeError,
                        &format!("Can only join strings, got `{}`.", item.to_string(&vm.heap)),
                    )
                    .unwrap_err());
            }
        }
    }

    let result = parts.join(&sep);
    Ok(vm.heap.string_id(&result).into())
}

/// `String.strip()` - trim leading/trailing whitespace.
///
/// TODO: Add `rstrip`, `lstrip` and single arg versions
/// `strip_bytes`, `strip_chars`, `strip_clusters` as well as their l and r variants.
pub(super) fn string_strip_native(
    vm: &mut VM,
    receiver: &Value,
    _args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);

    let result = s.trim().to_string();

    Ok(vm.heap.string_id(&result).into())
}

/// `String.startswith(prefix)` - check if self starts with prefix.
pub(super) fn string_startswith_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    match &args[0] {
        Value::String(prefix_id) => {
            let prefix = prefix_id.as_normalized_string(&vm.heap);
            Ok(Value::Bool(s.starts_with(&prefix)))
        }
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected a string argument, got `{}`.",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// `String.endswith(suffix)` - check if self ends with prefix.
pub(super) fn string_endswith_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    match &args[0] {
        Value::String(suffix_id) => {
            let suffix = suffix_id.as_normalized_string(&vm.heap);
            Ok(Value::Bool(s.ends_with(&suffix)))
        }
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected a string argument, got `{}`.",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// `String.removeprefix(prefix)` - remove prefix from the string if it starts with it.
pub(super) fn string_removeprefix_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    match &args[0] {
        Value::String(prefix_id) => {
            let prefix = prefix_id.as_normalized_string(&vm.heap);
            Ok(vm
                .heap
                .string_id(&s.strip_prefix(&prefix).unwrap_or(&s))
                .into())
        }
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected a string argument, got `{}`.",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

/// `String.removesuffix(suffix)` - remove suffix from the string if it starts with it.
pub(super) fn string_removesuffix_native(
    vm: &mut VM,
    receiver: &Value,
    args: &[Value],
) -> VmResult<Value> {
    let s = receiver.as_string().as_normalized_string(&vm.heap);
    match &args[0] {
        Value::String(suffix_id) => {
            let suffix = suffix_id.as_normalized_string(&vm.heap);
            Ok(vm
                .heap
                .string_id(&s.strip_suffix(&suffix).unwrap_or(&s))
                .into())
        }
        x => Err(vm
            .throw(
                TypeError,
                &format!(
                    "Expected a string argument, got `{}`.",
                    x.to_string(&vm.heap)
                ),
            )
            .unwrap_err()),
    }
}

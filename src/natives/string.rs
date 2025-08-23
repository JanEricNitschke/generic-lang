use crate::value::{GenericInt, Instance, List, NativeClass, Number, Value};
use crate::vm::VM;
use crate::vm::errors::VmResult;

use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

/// String.__init__(value) - Convert any value to string using `to_string_native` logic
pub(super) fn string_init_native(
    vm: &mut VM,
    _receiver: &mut Value,
    args: &mut [&mut Value],
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
        Ok(Value::String(vm.value_to_string(args[0])?))
    }
}

// String.replace(old, new) — normalized
pub(super) fn string_replace_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let s = receiver
        .as_string()
        .to_value(&vm.heap)
        .nfc()
        .collect::<String>();
    match args {
        [Value::String(old), Value::String(new)] => {
            let old = old.to_value(&vm.heap).nfc().collect::<String>();
            let new = new.to_value(&vm.heap).nfc().collect::<String>();

            Ok(Value::String(vm.heap.string_id(&s.replace(&old, &new))))
        }
        _ => Err(vm
            .throw_type_error(&format!(
                "Expected two strings, got {} and {}",
                args[0].to_string(&vm.heap),
                args[1].to_string(&vm.heap),
            ))
            .unwrap_err()),
    }
}

/// String.contains(substring) — normalized
pub(super) fn string_contains_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let s = receiver
        .as_string()
        .to_value(&vm.heap)
        .nfc()
        .collect::<String>();
    if let Value::String(sub) = args[0] {
        let sub = sub.to_value(&vm.heap).nfc().collect::<String>();
        Ok(Value::Bool(s.contains(&sub)))
    } else {
        Err(vm
            .throw_type_error(&format!(
                "Expected string argument, got {}",
                args[0].to_string(&vm.heap)
            ))
            .unwrap_err())
    }
}

/// String.find(substring) — normalized
pub(super) fn string_find_native(
    vm: &mut VM,
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let s = receiver
        .as_string()
        .to_value(&vm.heap)
        .nfc()
        .collect::<String>();
    if let Value::String(sub) = args[0] {
        let sub = sub.to_value(&vm.heap).nfc().collect::<String>();
        Ok(Value::Number(Number::Integer(
            s.find(&sub).map_or(GenericInt::Small(-1), |u| {
                GenericInt::from_usize(u, &mut vm.heap)
            }),
        )))
    } else {
        Err(vm
            .throw_type_error(&format!(
                "Expected string argument, got {}",
                args[0].to_string(&vm.heap)
            ))
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
    receiver: &mut Value,
    _args: &mut [&mut Value],
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
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw_value_error(&format!(
                        "Can not index into bytes with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw_type_error(&format!(
                    "Can only index into bytes with integer, got `{}`.",
                    x.to_string(&vm.heap)
                ))
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap);
    match s.as_bytes().get(index) {
        Some(b) => Ok(Value::Number(Number::Integer(GenericInt::Small(
            (*b).into(),
        )))),
        None => Err(vm
            .throw_index_error("byte index out of bounds")
            .unwrap_err()),
    }
}

pub(super) fn string_chars_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
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
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw_value_error(&format!(
                        "Can not index into chars with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw_type_error(&format!(
                    "Can only index into chars with integer, got `{}`.",
                    x.to_string(&vm.heap)
                ))
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap);
    match s.chars().nth(index) {
        Some(c) => Ok(Value::String(vm.heap.string_id(&c.to_string()))),
        None => Err(vm
            .throw_index_error("char index out of bounds")
            .unwrap_err()),
    }
}

pub(super) fn string_clusters_native(
    vm: &mut VM,
    receiver: &mut Value,
    _args: &mut [&mut Value],
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
    receiver: &mut Value,
    args: &mut [&mut Value],
) -> VmResult<Value> {
    let index = match &args[0] {
        Value::Number(Number::Integer(n)) => match n.try_to_usize(&vm.heap) {
            Ok(index) => index,
            Err(_) => {
                return Err(vm
                    .throw_value_error(&format!(
                        "Can not index into clusters with negative or too large numbers, got `{}`.",
                        n.to_string(&vm.heap)
                    ))
                    .unwrap_err());
            }
        },
        x => {
            return Err(vm
                .throw_type_error(&format!(
                    "Can only index into clusters with integer, got `{}`.",
                    x.to_string(&vm.heap)
                ))
                .unwrap_err());
        }
    };
    let s = receiver.as_string().to_value(&vm.heap).clone();
    match s.graphemes(true).nth(index) {
        Some(cluster) => Ok(Value::String(vm.heap.string_id(&cluster))),
        None => Err(vm
            .throw_index_error("cluster index out of bounds")
            .unwrap_err()),
    }
}

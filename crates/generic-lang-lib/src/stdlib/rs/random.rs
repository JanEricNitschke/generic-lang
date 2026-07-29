//! The `random` stdlib module: a shared, seedable pseudo-random source.
//!
//! The generator lives on the VM (`vm.rng`), seeded from the operating
//! system at construction and replaceable with `seed(n)` for reproducible
//! runs. Every native here already holds `&mut VM`, so it draws directly.
#![allow(clippy::unnecessary_wraps)]

use rand::distr::{Distribution, Uniform};
use rand::rngs::StdRng;
use rand::seq::{IndexedRandom, SliceRandom};
use rand::{RngExt, SeedableRng};

use crate::heap::InstanceId;
use crate::value::{
    GenericInt, Instance, List, ModuleContents, ModuleExport, NativeClass, Number, Value,
};
use crate::vm::ExceptionKind::{TypeError, ValueError};
use crate::vm::VM;
use crate::vm::errors::VmResult;

/// The instance id of a list argument, or a `TypeError` naming `what`. The
/// caller borrows the list out of the heap itself, so nothing is cloned.
fn as_list_id(vm: &mut VM, value: Value, what: &str) -> VmResult<InstanceId> {
    if let Value::Instance(id) = value
        && matches!(id.to_value(&vm.heap).backing, Some(NativeClass::List(_)))
    {
        Ok(id)
    } else {
        let rendered = value.to_string(&vm.heap);
        Err(vm
            .throw(
                TypeError,
                &format!("'{what}' expects a list, got: {rendered}"),
            )
            .unwrap_err())
    }
}

/// `random()` - a float in `[0.0, 1.0)`.
fn random_native(vm: &mut VM, _args: &[Value]) -> VmResult<Value> {
    let value: f64 = vm.rng.random();
    Ok(value.into())
}

/// `uniform(a, b)` - a float in `[a, b]` (either bound may be larger).
fn uniform_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let (Value::Number(a), Value::Number(b)) = (&args[0], &args[1]) else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'uniform' expects two numbers, got: {} and {}",
                    args[0].to_string(&vm.heap),
                    args[1].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let (low, high) = (a.to_f64(&vm.heap), b.to_f64(&vm.heap));
    let (low, high) = if low <= high {
        (low, high)
    } else {
        (high, low)
    };
    // Rejects NaN bounds (they order as an empty range), non-finite
    // bounds, and finite bounds whose span overflows to infinity.
    let Ok(distribution) = Uniform::new_inclusive(low, high) else {
        return Err(vm
            .throw(
                ValueError,
                "'uniform' bounds must be finite and their span must not overflow",
            )
            .unwrap_err());
    };
    Ok(distribution.sample(&mut vm.rng).into())
}

/// `randint(a, b)` - an integer in `[a, b]` inclusive. Bounds must fit in a
/// 64-bit integer; wider big-int bounds are rejected.
fn randint_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let (Value::Number(Number::Integer(low)), Value::Number(Number::Integer(high))) =
        (&args[0], &args[1])
    else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'randint' expects two integers, got: {} and {}",
                    args[0].to_string(&vm.heap),
                    args[1].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let (GenericInt::Small(low), GenericInt::Small(high)) = (low, high) else {
        return Err(vm
            .throw(ValueError, "'randint' bounds must fit in a 64-bit integer")
            .unwrap_err());
    };
    if low > high {
        return Err(vm
            .throw(
                ValueError,
                "'randint' requires the low bound not exceed the high bound",
            )
            .unwrap_err());
    }
    Ok(vm.rng.random_range(*low..=*high).into())
}

/// `choice(list)` - a random element of a non-empty list.
fn choice_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let id = as_list_id(vm, args[0], "choice")?;
    let chosen = {
        let Some(NativeClass::List(list)) = &id.to_value(&vm.heap).backing else {
            unreachable!("as_list_id guarantees a list backing")
        };
        list.items.choose(&mut vm.rng).copied()
    };
    match chosen {
        Some(item) => Ok(item),
        None => Err(vm
            .throw(ValueError, "'choice' cannot choose from an empty list")
            .unwrap_err()),
    }
}

/// `shuffle(list)` - shuffle the list in place; returns nil.
fn shuffle_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let id = as_list_id(vm, args[0], "shuffle")?;
    // `heap` and `rng` are disjoint VM fields, so the list borrow and the
    // generator borrow coexist - no clone needed to shuffle in place.
    if let Some(NativeClass::List(list)) = &mut id.to_value_mut(&mut vm.heap).backing {
        list.items.shuffle(&mut vm.rng);
    }
    Ok(Value::Nil)
}

/// `sample(list, k)` - a list of `k` distinct elements, in random order.
fn sample_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let id = as_list_id(vm, args[0], "sample")?;
    let Value::Number(Number::Integer(k)) = args[1] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'sample' expects an integer count, got: {}",
                    args[1].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    let len = {
        let Some(NativeClass::List(list)) = &id.to_value(&vm.heap).backing else {
            unreachable!("as_list_id guarantees a list backing")
        };
        list.items.len()
    };
    let k = match k {
        GenericInt::Small(n) => usize::try_from(n).ok(),
        GenericInt::Big(_) => None,
    }
    .filter(|k| *k <= len);
    let Some(k) = k else {
        return Err(vm
            .throw(
                ValueError,
                "'sample' count must be between 0 and the list length",
            )
            .unwrap_err());
    };
    let chosen: Vec<Value> = {
        let Some(NativeClass::List(list)) = &id.to_value(&vm.heap).backing else {
            unreachable!("as_list_id guarantees a list backing")
        };
        list.items.sample(&mut vm.rng, k).copied().collect()
    };
    let instance = Instance::new(
        *vm.heap.native_classes.get("List").unwrap(),
        Some(List::new(chosen).into()),
    );
    Ok(vm.heap.add_instance(instance))
}

/// `seed(n)` - reseed the VM's generator for reproducible sequences.
fn seed_native(vm: &mut VM, args: &[Value]) -> VmResult<Value> {
    let Value::Number(Number::Integer(seed)) = args[0] else {
        return Err(vm
            .throw(
                TypeError,
                &format!(
                    "'seed' expects an integer, got: {}",
                    args[0].to_string(&vm.heap)
                ),
            )
            .unwrap_err());
    };
    // A wrapping fold of the seed's bytes covers big-int seeds too.
    let bytes = seed.to_bigint(&vm.heap).to_signed_bytes_le();
    let mut state: u64 = 0;
    for byte in bytes {
        state = state.rotate_left(8) ^ u64::from(byte);
    }
    vm.rng = StdRng::seed_from_u64(state);
    Ok(Value::Nil)
}

pub(super) fn register(vm: &mut VM) {
    vm.register_stdlib_module(&"random", module());
}

/// Export all the contents of the module with the
/// name they are to be accessed with from generic; functions
/// additionally carry their supported arities.
fn module() -> ModuleContents {
    vec![
        ModuleExport::Function {
            name: "random",
            arity: &[0],
            fun: random_native,
        },
        ModuleExport::Function {
            name: "uniform",
            arity: &[2],
            fun: uniform_native,
        },
        ModuleExport::Function {
            name: "randint",
            arity: &[2],
            fun: randint_native,
        },
        ModuleExport::Function {
            name: "choice",
            arity: &[1],
            fun: choice_native,
        },
        ModuleExport::Function {
            name: "shuffle",
            arity: &[1],
            fun: shuffle_native,
        },
        ModuleExport::Function {
            name: "sample",
            arity: &[2],
            fun: sample_native,
        },
        ModuleExport::Function {
            name: "seed",
            arity: &[1],
            fun: seed_native,
        },
    ]
}

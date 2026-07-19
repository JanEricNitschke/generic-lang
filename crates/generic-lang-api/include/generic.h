/* Plugin ABI of the generic programming language. GENERATED — do not edit. */

#ifndef GENERIC_H
#define GENERIC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Version of the plugin ABI described by this crate.
 *
 * The host checks a module's [`ModuleDesc::abi_version`] before calling
 * anything in it and refuses to load mismatching plugins.
 */
#define GENERIC_PLUGIN_ABI_VERSION 1

/**
 * Discriminator for [`FfiReturn::status`].
 *
 * On the wire the status is a plain `u32` (an arbitrary integer from a
 * plugin must not become a Rust enum); decode with
 * [`FfiStatus::from_u32`], encode with `as u32`.
 */
enum GenericFfiStatus
#if defined(__cplusplus) || __STDC_VERSION__ >= 202311L
  : uint32_t
#endif // defined(__cplusplus) || __STDC_VERSION__ >= 202311L
 {
  /**
   * Success — `value` is the call's result.
   */
  GENERIC_FFI_STATUS_OK = 0,
  /**
   * `value` is the exception *instance*.
   *
   * From a host callback this is the exception generic code raised,
   * handed over with full identity: returning the same value (the safe
   * wrapper's `?` does) re-raises exactly that exception — class,
   * fields, and original stack trace intact. To throw a fresh
   * exception, create the instance with [`HostApi::exception_new`] and
   * return it under this status; a caught one can be examined with
   * [`HostApi::is_instance`] against a class from
   * [`HostApi::builtin_get`].
   */
  GENERIC_FFI_STATUS_EXCEPTION = 1,
  /**
   * A fatal host runtime error passing through the plugin.
   *
   * Not an exception — it is uncatchable by design. A re-entering host
   * callback returns it when the interpreter hit a fatal error; the
   * plugin must forward it unchanged (the safe wrapper's `?` does), and
   * the host re-raises it as a fatal error when the plugin call
   * returns. `value` carries no meaning for this status.
   */
  GENERIC_FFI_STATUS_FATAL = UINT32_MAX,
};
#ifndef __cplusplus
#if __STDC_VERSION__ >= 202311L
typedef enum GenericFfiStatus GenericFfiStatus;
#else
typedef uint32_t GenericFfiStatus;
#endif // __STDC_VERSION__ >= 202311L
#endif // __cplusplus

/**
 * Value kinds returned by [`HostApi::value_kind`].
 *
 * Over the FFI these travel as plain `u32` — convert with `as u32` /
 * [`ValueKind::from_u32`]. The host-side mapping (and the coverage test
 * guarding that every interpreter value maps to one of these) lands with
 * the feature-gated plugin module in the interpreter crate.
 */
enum GenericValueKind
#if defined(__cplusplus) || __STDC_VERSION__ >= 202311L
  : uint32_t
#endif // defined(__cplusplus) || __STDC_VERSION__ >= 202311L
 {
  /**
   * The `nil` value.
   */
  GENERIC_VALUE_KIND_NIL = 0,
  /**
   * A boolean.
   */
  GENERIC_VALUE_KIND_BOOL = 1,
  /**
   * An integer that fits in an `i64` (`int_get` succeeds).
   */
  GENERIC_VALUE_KIND_INT = 2,
  /**
   * An integer that does not fit in an `i64` (`int_get` fails; use
   * `value_display`/`value_str`).
   */
  GENERIC_VALUE_KIND_BIG_INT = 3,
  /**
   * A float.
   */
  GENERIC_VALUE_KIND_FLOAT = 4,
  /**
   * A rational number.
   */
  GENERIC_VALUE_KIND_RATIONAL = 5,
  /**
   * A string (`string_get` succeeds).
   */
  GENERIC_VALUE_KIND_STRING = 6,
  /**
   * A list (`list_len`/`list_get` succeed).
   */
  GENERIC_VALUE_KIND_LIST = 7,
  /**
   * A tuple.
   */
  GENERIC_VALUE_KIND_TUPLE = 8,
  /**
   * A dict.
   */
  GENERIC_VALUE_KIND_DICT = 9,
  /**
   * A set.
   */
  GENERIC_VALUE_KIND_SET = 10,
  /**
   * A range.
   */
  GENERIC_VALUE_KIND_RANGE = 11,
  /**
   * The `StopIteration` sentinel — what `__next__` returns when an
   * iterator is exhausted.
   */
  GENERIC_VALUE_KIND_STOP_ITERATION = 12,
  /**
   * A plain class instance (fields via `attr_get`/`attr_set`, methods
   * via `invoke_method`).
   */
  GENERIC_VALUE_KIND_INSTANCE = 13,
  /**
   * A class (instantiate via `call_value`).
   */
  GENERIC_VALUE_KIND_CLASS = 14,
  /**
   * A callable function value (closure, native function, or bound
   * method) — use `call_value`.
   */
  GENERIC_VALUE_KIND_FUNCTION = 15,
  /**
   * A module.
   */
  GENERIC_VALUE_KIND_MODULE = 16,
  /**
   * An exception instance.
   */
  GENERIC_VALUE_KIND_EXCEPTION = 17,
  /**
   * A generator.
   */
  GENERIC_VALUE_KIND_GENERATOR = 18,
  /**
   * A list/tuple/range/template iterator (drive via `invoke_method`
   * with `__next__`).
   */
  GENERIC_VALUE_KIND_ITERATOR = 19,
  /**
   * VM-internal values a plugin should never meaningfully receive.
   */
  GENERIC_VALUE_KIND_OTHER = 20,
};
#ifndef __cplusplus
#if __STDC_VERSION__ >= 202311L
typedef enum GenericValueKind GenericValueKind;
#else
typedef uint32_t GenericValueKind;
#endif // __STDC_VERSION__ >= 202311L
#endif // __cplusplus

/**
 * An opaque generic runtime value.
 *
 * This is the host's 32-byte `Value` bit-copied — discriminant and payload
 * included. Plugins must never inspect or fabricate its bytes; values are
 * opaque handles to be passed back to host callbacks. Use
 * [`HostApi::value_kind`] to ask what a value holds.
 */
typedef struct GenericValue {
  /**
   * Opaque storage. The limbs are [`MaybeUninit`] because a host `Value`
   * does not initialize all 32 bytes — small enum variants leave the
   * rest unwritten — and bit-copying it in must not assert those bytes
   * are initialized (that would be undefined behavior). `u64` limbs give
   * the type the host `Value`'s 8-byte alignment; it renders as
   * `uint64_t opaque[4]` in C. Never inspect.
   */
  uint64_t opaque[4];
} GenericValue;

/**
 * A borrowed UTF-8 string. Not NUL-terminated.
 *
 * Lifetime rules: an `FfiStr` returned by a host callback stays valid until
 * the next *re-entering* callback (see the rooting contract); an `FfiStr`
 * passed to a host callback only needs to be valid for that call.
 */
typedef struct FfiStr {
  /**
   * Pointer to the first byte (may be null only for the empty sentinel).
   */
  const uint8_t *ptr;
  /**
   * Length in bytes.
   */
  size_t len;
} FfiStr;

/**
 * Result of a plugin function or a re-entering host callback.
 *
 * `value` is always present; `status` (a [`FfiStatus`] as `u32`) says
 * what it is: the call's result, an exception instance to (re-)raise, or
 * a meaningless placeholder accompanying a fatal pass-through error.
 */
typedef struct FfiReturn {
  /**
   * A [`FfiStatus`] as `u32`: what `value` means.
   */
  uint32_t status;
  /**
   * The result, the exception instance, or a fatal-status placeholder.
   */
  struct GenericValue value;
} FfiReturn;

/**
 * The host vtable handed to every plugin call.
 *
 * `ctx` is an opaque pointer owned by the host; pass it as the first
 * argument to every callback. Callbacks marked **re-entering** run generic
 * bytecode, during which garbage collection may occur — see the rooting
 * contract: across a re-entering callback, `root` every value you still
 * hold and re-fetch any [`FfiStr`] afterward. All other callbacks never
 * trigger collection.
 *
 * Return conventions, decided solely by whether the payload forces an
 * out-parameter:
 * - A payload the caller must receive as something other than a
 *   [`GenericValue`] — a raw machine scalar (`bool`, `i64`, `f64`,
 *   `usize`) or a borrowed [`FfiStr`] — cannot ride in an [`FfiReturn`]
 *   (whose payload is a [`GenericValue`]), so it travels through an
 *   out-parameter and the callback returns a plain `bool` — `true` on
 *   success, `false` on the sole "wrong kind" failure. These carry no
 *   exception.
 * - Everything else — payload is a [`GenericValue`], or there is no
 *   payload — returns [`FfiReturn`], carrying a real exception instance on
 *   failure whose class and message mirror what the equivalent generic
 *   operation would throw.
 * - Infallible callbacks return their value directly.
 */
typedef struct HostApi {
  /**
   * ABI version of the host ([`GENERIC_PLUGIN_ABI_VERSION`]).
   */
  uint32_t abi_version;
  /**
   * Opaque host context; pass it back as the first argument of every
   * callback. Never dereference it.
   */
  void *ctx;
  /**
   * Kind of the value, as a [`ValueKind`](crate::ValueKind) code.
   */
  uint32_t (*value_kind)(void *ctx, struct GenericValue value);
  /**
   * `false` if the value is not a bool.
   */
  bool (*bool_get)(void *ctx, struct GenericValue value, bool *out);
  /**
   * Read an integer into `out`; `false` if the value is not an integer
   * or does not fit in an `i64` (big integers — fall back to
   * `value_display`).
   */
  bool (*int_get)(void *ctx, struct GenericValue value, int64_t *out);
  /**
   * `false` if the value is not a float.
   */
  bool (*float_get)(void *ctx, struct GenericValue value, double *out);
  /**
   * Read the interned bytes of a string value into `out` (valid until
   * the next re-entering callback); `false` if the value is not a
   * string.
   */
  bool (*string_get)(void *ctx, struct GenericValue value, struct FfiStr *out);
  /**
   * `false` if the value is not a list.
   */
  bool (*list_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * The element at `index`. `TypeError` if the value is not a list;
   * `IndexError` if the index is out of bounds.
   */
  struct FfiReturn (*list_get)(void *ctx, struct GenericValue value, size_t index);
  /**
   * `false` if the value is not a tuple.
   */
  bool (*tuple_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * The element at `index`. `TypeError` if the value is not a tuple;
   * `IndexError` if the index is out of bounds.
   */
  struct FfiReturn (*tuple_get)(void *ctx, struct GenericValue value, size_t index);
  /**
   * `false` if the value is not a dict.
   */
  bool (*dict_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * `false` if the value is not a set.
   */
  bool (*set_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * Look up a builtin global by name (exception classes like
   * `"TypeError"`, native classes, builtin functions). `NameError` if
   * absent; `TypeError` if the name is invalid UTF-8.
   */
  struct FfiReturn (*builtin_get)(void *ctx, struct FfiStr name);
  /**
   * Whether `value` is an instance of `of_class` or of a subclass of it
   * (a bool value on success) — the exact semantics of the `isinstance`
   * builtin, value-type proxy classes included. `TypeError` if
   * `of_class` is not a class.
   */
  struct FfiReturn (*is_instance)(void *ctx, struct GenericValue value, struct GenericValue of_class);
  /**
   * A field of an instance; `AttributeError` if absent, `TypeError` if
   * the receiver is not an instance.
   */
  struct FfiReturn (*attr_get)(void *ctx, struct GenericValue receiver, struct FfiStr name);
  /**
   * Set a field on an instance; `TypeError` if the receiver is not an
   * instance (the ok value is nil).
   */
  struct FfiReturn (*attr_set)(void *ctx,
                               struct GenericValue receiver,
                               struct FfiStr name,
                               struct GenericValue value);
  /**
   * Whether an instance has a field (a bool value on success).
   * `TypeError` if the receiver is not an instance or the name is
   * invalid UTF-8.
   */
  struct FfiReturn (*attr_has)(void *ctx, struct GenericValue receiver, struct FfiStr name);
  /**
   * A new nil value.
   */
  struct GenericValue (*nil_new)(void *ctx);
  /**
   * A new bool value.
   */
  struct GenericValue (*bool_new)(void *ctx, bool value);
  /**
   * A new integer value.
   */
  struct GenericValue (*int_new)(void *ctx, int64_t value);
  /**
   * A new float value.
   */
  struct GenericValue (*float_new)(void *ctx, double value);
  /**
   * Interns the given UTF-8 bytes into a string value; `ValueError` on
   * invalid UTF-8.
   */
  struct FfiReturn (*string_new)(void *ctx, struct FfiStr value);
  /**
   * A new, empty list.
   */
  struct GenericValue (*list_new)(void *ctx);
  /**
   * Append to a list (the ok value is nil); `TypeError` if the target is
   * not a list.
   */
  struct FfiReturn (*list_push)(void *ctx, struct GenericValue list, struct GenericValue item);
  /**
   * Replace the element at an index (the ok value is nil). `TypeError`
   * if the target is not a list; `IndexError` if the index is out of
   * bounds.
   */
  struct FfiReturn (*list_set)(void *ctx,
                               struct GenericValue list,
                               size_t index,
                               struct GenericValue value);
  /**
   * A new exception instance of `of_class` carrying `message`.
   * `TypeError` if `of_class` is not a class deriving from `Exception`
   * or the message is invalid UTF-8. Sets the message directly,
   * bypassing the class's `__init__` — exactly like the VM's own throw;
   * use `call_value` on the class for full construction semantics.
   * Return the instance under [`FfiStatus::Exception`] to throw it.
   */
  struct FfiReturn (*exception_new)(void *ctx, struct GenericValue of_class, struct FfiStr message);
  /**
   * The raw string representation of any value, as a new string value.
   * Does NOT honor a user class's `__str__` (use `value_str` for that),
   * which makes it safe to call anywhere, including error paths.
   */
  struct GenericValue (*value_display)(void *ctx, struct GenericValue value);
  /**
   * Call a callable value (closure, native, class, …) with the given
   * arguments. Generic exceptions come back as a nonzero status.
   */
  struct FfiReturn (*call_value)(void *ctx,
                                 struct GenericValue callee,
                                 const struct GenericValue *args,
                                 size_t nargs);
  /**
   * Invoke a named method on a receiver.
   */
  struct FfiReturn (*invoke_method)(void *ctx,
                                    struct GenericValue receiver,
                                    struct FfiStr name,
                                    const struct GenericValue *args,
                                    size_t nargs);
  /**
   * String conversion honoring a user class's `__str__`.
   */
  struct FfiReturn (*value_str)(void *ctx, struct GenericValue value);
  /**
   * Look up a key (`KeyError` if absent); re-enters for `__hash__`/`__eq__`.
   */
  struct FfiReturn (*dict_get)(void *ctx, struct GenericValue dict, struct GenericValue key);
  /**
   * Insert or replace a key (the ok value is nil).
   */
  struct FfiReturn (*dict_set)(void *ctx,
                               struct GenericValue dict,
                               struct GenericValue key,
                               struct GenericValue value);
  /**
   * Whether a dict contains a key (the ok value is a bool).
   */
  struct FfiReturn (*dict_contains)(void *ctx, struct GenericValue dict, struct GenericValue key);
  /**
   * Add an item to a set (the ok value is nil).
   */
  struct FfiReturn (*set_add)(void *ctx, struct GenericValue set, struct GenericValue item);
  /**
   * Whether a set contains an item (the ok value is a bool).
   */
  struct FfiReturn (*set_contains)(void *ctx, struct GenericValue set, struct GenericValue item);
  /**
   * Truthiness honoring `__bool__` (the ok value is a bool).
   */
  struct FfiReturn (*value_truthy)(void *ctx, struct GenericValue value);
  /**
   * Equality honoring `__eq__` (the ok value is a bool).
   */
  struct FfiReturn (*value_equals)(void *ctx, struct GenericValue a, struct GenericValue b);
  /**
   * Hash honoring `__hash__` (the ok value is an integer).
   */
  struct FfiReturn (*value_hash)(void *ctx, struct GenericValue value);
  /**
   * Keep a value alive across re-entering callbacks. Roots are released
   * automatically when the plugin function returns; `unroot` releases
   * the `n` most recent roots early.
   */
  void (*root)(void *ctx, struct GenericValue value);
  /**
   * Release the `n` most recently rooted values.
   */
  void (*unroot)(void *ctx, size_t n);
} HostApi;

/**
 * The signature every exported plugin function has.
 *
 * `args` points at `nargs` contiguous values owned by the host; they stay
 * valid (and GC-rooted) for the whole call.
 */
typedef struct FfiReturn (*PluginFn)(const struct HostApi *host,
                                     const struct GenericValue *args,
                                     size_t nargs);

/**
 * Description of one exported plugin function.
 */
typedef struct FunctionDesc {
  /**
   * Function name as seen from generic code.
   */
  struct FfiStr name;
  /**
   * Accepted argument counts (the host checks arity before calling).
   */
  const uint8_t *arities;
  /**
   * Number of entries in `arities`.
   */
  size_t arities_len;
  /**
   * The function implementation.
   */
  PluginFn fun;
} FunctionDesc;

/**
 * Description of a plugin module; returned by `generic_plugin_init`, the
 * one symbol every plugin must export:
 *
 * ```c
 * const ModuleDesc *generic_plugin_init(void);
 * ```
 */
typedef struct ModuleDesc {
  /**
   * ABI version the plugin was built against ([`GENERIC_PLUGIN_ABI_VERSION`]).
   */
  uint32_t abi_version;
  /**
   * Pointer to `functions_len` contiguous [`FunctionDesc`] entries.
   */
  const struct FunctionDesc *functions;
  /**
   * Number of entries in `functions`.
   */
  size_t functions_len;
} ModuleDesc;

#endif  /* GENERIC_H */

#ifdef __cplusplus
extern "C" {
#endif  // __cplusplus

/**
 * The one symbol every plugin must export.
 */
const ModuleDesc *generic_plugin_init(void);

#ifdef __cplusplus
}  // extern "C"
#endif  // __cplusplus

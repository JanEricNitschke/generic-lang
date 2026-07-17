/* Plugin ABI of the generic programming language. GENERATED — do not edit. */

#ifndef GENERIC_PLUGIN_H
#define GENERIC_PLUGIN_H

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
 * Exception-kind codes carried in [`FfiReturn::status`].
 *
 * `0` is reserved for "no exception" and is deliberately not a variant.
 * The discriminants duplicate the interpreter's `ExceptionKind` enum
 * (`crates/generic-lang-lib/src/vm/exception_handling.rs`), which is the
 * source of truth; a sync test in the interpreter crate guards the
 * duplication. Unknown codes are treated as the base `Exception`.
 *
 * Over the FFI these travel as plain `u32` — convert with `as u32` /
 * [`ExceptionCode::from_u32`].
 */
enum GenericExceptionCode
#if defined(__cplusplus) || __STDC_VERSION__ >= 202311L
  : uint32_t
#endif // defined(__cplusplus) || __STDC_VERSION__ >= 202311L
 {
  /**
   * The base exception class; also what runtime errors throw.
   */
  GENERIC_EXCEPTION_CODE_EXCEPTION = 1,
  GENERIC_EXCEPTION_CODE_TYPE_ERROR = 2,
  GENERIC_EXCEPTION_CODE_VALUE_ERROR = 3,
  GENERIC_EXCEPTION_CODE_NAME_ERROR = 4,
  GENERIC_EXCEPTION_CODE_CONST_REASSIGNMENT_ERROR = 5,
  GENERIC_EXCEPTION_CODE_ATTRIBUTE_ERROR = 6,
  GENERIC_EXCEPTION_CODE_IMPORT_ERROR = 7,
  GENERIC_EXCEPTION_CODE_ASSERTION_ERROR = 8,
  GENERIC_EXCEPTION_CODE_IO_ERROR = 9,
  GENERIC_EXCEPTION_CODE_KEY_ERROR = 10,
  GENERIC_EXCEPTION_CODE_INDEX_ERROR = 11,
};
#ifndef __cplusplus
#if __STDC_VERSION__ >= 202311L
typedef enum GenericExceptionCode GenericExceptionCode;
#else
typedef uint32_t GenericExceptionCode;
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
 * handles to be passed back to host callbacks (the `PyObject*` / Lua-handle
 * pattern). Use [`HostApi::value_kind`] to ask what a value holds.
 */
typedef struct GenericValue {
  /**
   * Opaque storage — represented as `u64`s purely so the type has the
   * host `Value`'s 8-byte alignment in every language. Never inspect.
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
  const uint8_t *ptr;
  size_t len;
} FfiStr;

/**
 * Result of a plugin function or a re-entering host callback.
 *
 * `status == 0` means success and `value` is the result. Any other status
 * is an exception-kind code from [`ExceptionCode`](crate::ExceptionCode)
 * and `value` is the message (a string value); unknown codes are treated as
 * the base `Exception`.
 */
typedef struct FfiReturn {
  uint32_t status;
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
 */
typedef struct HostApi {
  uint32_t abi_version;
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
   * `false` if the value is not an integer or does not fit in an `i64`
   * (big integers; fall back to `value_display`).
   */
  bool (*int_get)(void *ctx, struct GenericValue value, int64_t *out);
  /**
   * `false` if the value is not a float.
   */
  bool (*float_get)(void *ctx, struct GenericValue value, double *out);
  /**
   * The interned bytes of a string value; [`FfiStr::null`] if the value
   * is not a string. Valid until the next re-entering callback.
   */
  struct FfiStr (*string_get)(void *ctx, struct GenericValue value);
  /**
   * `false` if the value is not a list.
   */
  bool (*list_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * `false` if the value is not a list or the index is out of bounds.
   */
  bool (*list_get)(void *ctx, struct GenericValue value, size_t index, struct GenericValue *out);
  /**
   * `false` if the value is not a tuple.
   */
  bool (*tuple_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * `false` if the value is not a tuple or the index is out of bounds.
   */
  bool (*tuple_get)(void *ctx, struct GenericValue value, size_t index, struct GenericValue *out);
  /**
   * `false` if the value is not a dict.
   */
  bool (*dict_len)(void *ctx, struct GenericValue value, size_t *out);
  /**
   * `false` if the value is not a set.
   */
  bool (*set_len)(void *ctx, struct GenericValue value, size_t *out);
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
   * Whether an instance has a field; `false` (the return, not `out`) if
   * the receiver is not an instance.
   */
  bool (*attr_has)(void *ctx, struct GenericValue receiver, struct FfiStr name, bool *out);
  struct GenericValue (*nil_new)(void *ctx);
  struct GenericValue (*bool_new)(void *ctx, bool value);
  struct GenericValue (*int_new)(void *ctx, int64_t value);
  struct GenericValue (*float_new)(void *ctx, double value);
  /**
   * Interns the given UTF-8 bytes; `false` on invalid UTF-8.
   */
  bool (*string_new)(void *ctx, struct FfiStr value, struct GenericValue *out);
  /**
   * A new, empty list.
   */
  struct GenericValue (*list_new)(void *ctx);
  /**
   * `false` if the target is not a list.
   */
  bool (*list_push)(void *ctx, struct GenericValue list, struct GenericValue item);
  /**
   * Replace the element at an index; `false` if the target is not a list
   * or the index is out of bounds.
   */
  bool (*list_set)(void *ctx, struct GenericValue list, size_t index, struct GenericValue value);
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
  size_t arities_len;
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
  uint32_t abi_version;
  const struct FunctionDesc *functions;
  size_t functions_len;
} ModuleDesc;

#endif  /* GENERIC_PLUGIN_H */

/**
 * The one symbol every plugin must export.
 */
const ModuleDesc *generic_plugin_init(void);

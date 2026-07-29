/* Tiny C test plugin - proves the plugin ABI is usable from plain C against
 * the generated `generic.h`, with no dependency on the Rust API crate.
 *
 * Registers under the module name `c_demo_plugin`. Built by the Makefile
 * `plugin-lang-fixture` step when a C compiler is present:
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/c_demo_plugin.<ext> plugin-examples/c/c_demo_plugin.c
 */
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "generic.h"

/* Build an exception instance of the named builtin class and return it under
 * the EXCEPTION status - the C analogue of `host.type_error(..)` in Rust.
 * Each host call can itself fail (EXCEPTION or FATAL); forward any non-OK
 * FfiReturn unchanged, immediately - never relabel or swallow it. */
static FfiReturn throw_new(const HostApi *host, const char *class_name, const char *msg) {
    FfiStr name = {.ptr = (const uint8_t *)class_name, .len = strlen(class_name)};
    FfiStr message = {.ptr = (const uint8_t *)msg, .len = strlen(msg)};
    FfiReturn cls = host->builtin_get(host->ctx, name);
    if (cls.status != GENERIC_FFI_STATUS_OK) {
        return cls;
    }
    FfiReturn exc = host->exception_new(host->ctx, cls.value, message);
    if (exc.status != GENERIC_FFI_STATUS_OK) {
        return exc;
    }
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_EXCEPTION, .value = exc.value};
    return ret;
}

static FfiReturn ok(GenericValue value) {
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = value};
    return ret;
}

/* add(a, b) - integer addition. */
static FfiReturn add(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)nargs; /* arity-checked by the host */
    int64_t a = 0, b = 0;
    if (!host->int_get(host->ctx, args[0], &a) || !host->int_get(host->ctx, args[1], &b)) {
        return throw_new(host, "TypeError", "c_demo_plugin.add expects two ints");
    }
    return ok(host->int_new(host->ctx, a + b));
}

/* shout(s) - ASCII-uppercase a string and append '!'. Demonstrates the string
 * round-trip: borrow the host bytes, build a new buffer, intern it. */
static FfiReturn shout(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)nargs;
    FfiStr s = {0};
    if (!host->string_get(host->ctx, args[0], &s)) {
        return throw_new(host, "TypeError", "c_demo_plugin.shout expects a string");
    }
    char *buf = (char *)malloc(s.len + 2);
    if (buf == NULL) {
        return throw_new(host, "Exception", "c_demo_plugin.shout out of memory");
    }
    for (size_t i = 0; i < s.len; i++) {
        buf[i] = (char)toupper((unsigned char)s.ptr[i]);
    }
    buf[s.len] = '!';
    FfiStr out = {.ptr = (const uint8_t *)buf, .len = s.len + 1};
    FfiReturn result = host->string_new(host->ctx, out);
    free(buf);
    if (result.status != GENERIC_FFI_STATUS_OK) {
        return result;
    }
    return ok(result.value);
}

/* raise(class_name, message) - throw a fresh instance of the named builtin
 * exception class. Exercises the throw path for any class from generic code. */
static FfiReturn raise(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)nargs;
    FfiStr class_name = {0}, message = {0};
    if (!host->string_get(host->ctx, args[0], &class_name)) {
        return throw_new(host, "TypeError", "c_demo_plugin.raise expects (class, message) strings");
    }
    /* string_get bytes are valid only until the next re-entering callback.
     * Nothing re-enters between here and the throw, but copying the class name
     * into our own buffer is the safe C habit (Rust enforces this at compile
     * time; in C it is on you). */
    char class_buf[64];
    size_t n = class_name.len < sizeof(class_buf) - 1 ? class_name.len : sizeof(class_buf) - 1;
    memcpy(class_buf, class_name.ptr, n);
    class_buf[n] = '\0';
    if (!host->string_get(host->ctx, args[1], &message)) {
        return throw_new(host, "TypeError", "c_demo_plugin.raise expects (class, message) strings");
    }
    char *msg_buf = (char *)malloc(message.len + 1);
    if (msg_buf == NULL) {
        return throw_new(host, "Exception", "c_demo_plugin.raise out of memory");
    }
    memcpy(msg_buf, message.ptr, message.len);
    msg_buf[message.len] = '\0';
    FfiReturn ret = throw_new(host, class_buf, msg_buf);
    free(msg_buf);
    return ret;
}

/* empty_string() - build "" inside the plugin: a valid empty FfiStr is a
 * non-null pointer with len 0 (a null pointer is not a valid string). */
static FfiReturn empty_string(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)args;
    (void)nargs;
    FfiStr out = {.ptr = (const uint8_t *)"", .len = 0};
    return host->string_new(host->ctx, out);
}

/* --- a plugin class -----------------------------------------------------
 *
 * `Counter` mirrors the Rust example: hidden native data (count), a managed
 * GenericValue (label) reported to the GC via counter_traverse, and a
 * generic-side attribute (note/origin) set via attr_set. Methods take the
 * receiver as a separate parameter; `args`/`nargs` are the remaining args, and
 * arities exclude the receiver. */

typedef struct {
    int64_t count;
    GenericValue label;
} CounterState;

/* Free the opaque state when a Counter (or subclass) is garbage-collected. */
static void counter_drop(void *opaque) { free(opaque); /* free(NULL) is a no-op */ }

/* Report the held label so the GC does not sweep it while the Counter lives. */
static int32_t counter_traverse(void *opaque, PluginVisitFn visit, void *visit_ctx) {
    if (opaque == NULL) {
        return 0;
    }
    CounterState *s = (CounterState *)opaque;
    visit(visit_ctx, s->label);
    return 0;
}

static CounterState *counter_state(const HostApi *host, GenericValue receiver) {
    return (CounterState *)host->instance_get_opaque(host->ctx, receiver);
}

/* Counter.__init__(label) - validate, allocate hidden state, set the `origin`
 * attribute, and (like every __init__) return the receiver. */
static FfiReturn counter_init(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)nargs;
    if (host->value_kind(host->ctx, args[0]) == GENERIC_VALUE_KIND_NIL) {
        return throw_new(host, "TypeError", "Counter label must not be nil");
    }
    CounterState *s = (CounterState *)malloc(sizeof *s);
    if (s == NULL) {
        return throw_new(host, "Exception", "Counter.__init__ out of memory");
    }
    s->count = 0;
    s->label = args[0];
    FfiReturn set = host->instance_set_opaque(host->ctx, receiver, s);
    if (set.status != GENERIC_FFI_STATUS_OK) {
        free(s);
        return set;
    }
    FfiStr origin_name = {.ptr = (const uint8_t *)"origin", .len = 6};
    FfiStr origin_val = {.ptr = (const uint8_t *)"counter", .len = 7};
    FfiReturn origin = host->string_new(host->ctx, origin_val);
    if (origin.status != GENERIC_FFI_STATUS_OK) {
        return origin;
    }
    FfiReturn aset = host->attr_set(host->ctx, receiver, origin_name, origin.value);
    if (aset.status != GENERIC_FFI_STATUS_OK) {
        return aset;
    }
    return ok(receiver);
}

static FfiReturn counter_increment(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)args;
    (void)nargs;
    CounterState *s = counter_state(host, receiver);
    if (s == NULL) {
        return throw_new(host, "TypeError", "Counter method on an uninitialized instance");
    }
    s->count += 1;
    return ok(host->int_new(host->ctx, s->count));
}

static FfiReturn counter_value(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)args;
    (void)nargs;
    CounterState *s = counter_state(host, receiver);
    if (s == NULL) {
        return throw_new(host, "TypeError", "Counter method on an uninitialized instance");
    }
    return ok(host->int_new(host->ctx, s->count));
}

static FfiReturn counter_label(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)args;
    (void)nargs;
    CounterState *s = counter_state(host, receiver);
    if (s == NULL) {
        return throw_new(host, "TypeError", "Counter method on an uninitialized instance");
    }
    return ok(s->label);
}

static FfiReturn counter_set_note(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)nargs;
    FfiStr note = {.ptr = (const uint8_t *)"note", .len = 4};
    FfiReturn set = host->attr_set(host->ctx, receiver, note, args[0]);
    if (set.status != GENERIC_FFI_STATUS_OK) {
        return set;
    }
    return ok(host->nil_new(host->ctx));
}

static FfiReturn counter_get_note(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)args;
    (void)nargs;
    FfiStr note = {.ptr = (const uint8_t *)"note", .len = 4};
    return host->attr_get(host->ctx, receiver, note);
}

/* Counter.__add__(other) - a dunder returning a new Counter holding the sum.
 * `other` is type-checked against the receiver's class before its opaque state
 * is read, so a foreign instance can never be misread as a CounterState. */
static FfiReturn counter_add(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)nargs;
    GenericValue other = args[0];
    FfiReturn cls = host->class_of(host->ctx, receiver);
    if (cls.status != GENERIC_FFI_STATUS_OK) {
        return cls;
    }
    FfiReturn is = host->is_instance(host->ctx, other, cls.value);
    if (is.status != GENERIC_FFI_STATUS_OK) {
        return is;
    }
    bool is_counter = false;
    if (!host->bool_get(host->ctx, is.value, &is_counter) || !is_counter) {
        return throw_new(host, "TypeError", "Counter.__add__ expects another Counter");
    }
    CounterState *a = counter_state(host, receiver);
    CounterState *b = counter_state(host, other);
    if (a == NULL || b == NULL) {
        return throw_new(host, "TypeError", "Counter method on an uninitialized instance");
    }
    int64_t sum = a->count + b->count;
    GenericValue label = a->label; /* read before re-entering call_value */
    FfiReturn made = host->call_value(host->ctx, cls.value, &label, 1);
    if (made.status != GENERIC_FFI_STATUS_OK) {
        return made;
    }
    CounterState *n = counter_state(host, made.value);
    if (n == NULL) {
        return throw_new(host, "Exception", "Counter.__add__ construction failed");
    }
    n->count = sum;
    return ok(made.value);
}

/* A second plugin class with a distinct opaque type, so tests can check that
 * `counter + ticket` is a clean TypeError rather than a type confusion. */
static void ticket_drop(void *opaque) { free(opaque); }

static FfiReturn ticket_init(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t nargs
) {
    (void)args;
    (void)nargs;
    int64_t *id = (int64_t *)malloc(sizeof *id);
    if (id == NULL) {
        return throw_new(host, "Exception", "Ticket.__init__ out of memory");
    }
    *id = 0;
    FfiReturn set = host->instance_set_opaque(host->ctx, receiver, id);
    if (set.status != GENERIC_FFI_STATUS_OK) {
        free(id);
        return set;
    }
    return ok(receiver);
}

static const uint8_t ARITY_0[] = {0};
static const uint8_t ARITY_1[] = {1};
static const uint8_t ARITY_2[] = {2};

static const MethodDesc COUNTER_METHODS[] = {
    {.name = {.ptr = (const uint8_t *)"__init__", .len = 8},
     .arities = ARITY_1,
     .arities_len = 1,
     .fun = counter_init},
    {.name = {.ptr = (const uint8_t *)"increment", .len = 9},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = counter_increment},
    {.name = {.ptr = (const uint8_t *)"value", .len = 5},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = counter_value},
    {.name = {.ptr = (const uint8_t *)"label", .len = 5},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = counter_label},
    {.name = {.ptr = (const uint8_t *)"set_note", .len = 8},
     .arities = ARITY_1,
     .arities_len = 1,
     .fun = counter_set_note},
    {.name = {.ptr = (const uint8_t *)"get_note", .len = 8},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = counter_get_note},
    {.name = {.ptr = (const uint8_t *)"__add__", .len = 7},
     .arities = ARITY_1,
     .arities_len = 1,
     .fun = counter_add},
};

static const MethodDesc TICKET_METHODS[] = {
    {.name = {.ptr = (const uint8_t *)"__init__", .len = 8},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = ticket_init},
};

static const ClassDesc CLASSES[] = {
    {.name = {.ptr = (const uint8_t *)"Counter", .len = 7},
     .methods = COUNTER_METHODS,
     .methods_len = sizeof COUNTER_METHODS / sizeof COUNTER_METHODS[0],
     .drop = counter_drop,
     .traverse = counter_traverse},
    {.name = {.ptr = (const uint8_t *)"Ticket", .len = 6},
     .methods = TICKET_METHODS,
     .methods_len = sizeof TICKET_METHODS / sizeof TICKET_METHODS[0],
     .drop = ticket_drop,
     .traverse = NULL},
};

/* speed_of_light - a module constant, built once when the module is
 * imported. Value creators receive only the host vtable. */
static FfiReturn make_speed_of_light(const HostApi *host) {
    return ok(host->int_new(host->ctx, 299792458));
}

static const ValueDesc VALUES[] = {
    {.name = {.ptr = (const uint8_t *)"speed_of_light", .len = 14}, .fun = make_speed_of_light},
};

static const FunctionDesc FUNCTIONS[] = {
    {.name = {.ptr = (const uint8_t *)"add", .len = 3},
     .arities = ARITY_2,
     .arities_len = 1,
     .fun = add},
    {.name = {.ptr = (const uint8_t *)"empty_string", .len = 12},
     .arities = ARITY_0,
     .arities_len = 1,
     .fun = empty_string},
    {.name = {.ptr = (const uint8_t *)"shout", .len = 5},
     .arities = ARITY_1,
     .arities_len = 1,
     .fun = shout},
    {.name = {.ptr = (const uint8_t *)"raise", .len = 5},
     .arities = ARITY_2,
     .arities_len = 1,
     .fun = raise},
};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = FUNCTIONS,
    .functions_len = sizeof FUNCTIONS / sizeof FUNCTIONS[0],
    .classes = CLASSES,
    .classes_len = sizeof CLASSES / sizeof CLASSES[0],
    .values = VALUES,
    .values_len = sizeof VALUES / sizeof VALUES[0],
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

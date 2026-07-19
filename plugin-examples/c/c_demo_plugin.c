/* Tiny C test plugin — proves the plugin ABI is usable from plain C against
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
 * the EXCEPTION status — the C analogue of `host.type_error(..)` in Rust. */
static FfiReturn throw_new(const HostApi *host, const char *class_name, const char *msg) {
    FfiStr name = {.ptr = (const uint8_t *)class_name, .len = strlen(class_name)};
    FfiStr message = {.ptr = (const uint8_t *)msg, .len = strlen(msg)};
    FfiReturn cls = host->builtin_get(host->ctx, name);
    FfiReturn exc = host->exception_new(host->ctx, cls.value, message);
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_EXCEPTION, .value = exc.value};
    return ret;
}

static FfiReturn ok(GenericValue value) {
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = value};
    return ret;
}

/* add(a, b) — integer addition. */
static FfiReturn add(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)nargs; /* arity-checked by the host */
    int64_t a = 0, b = 0;
    if (!host->int_get(host->ctx, args[0], &a) || !host->int_get(host->ctx, args[1], &b)) {
        return throw_new(host, "TypeError", "c_demo_plugin.add expects two ints");
    }
    return ok(host->int_new(host->ctx, a + b));
}

/* shout(s) — ASCII-uppercase a string and append '!'. Demonstrates the string
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

/* raise(class_name, message) — throw a fresh instance of the named builtin
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

static const uint8_t ARITY_1[] = {1};
static const uint8_t ARITY_2[] = {2};
static const FunctionDesc FUNCTIONS[] = {
    {.name = {.ptr = (const uint8_t *)"add", .len = 3},
     .arities = ARITY_2,
     .arities_len = 1,
     .fun = add},
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
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

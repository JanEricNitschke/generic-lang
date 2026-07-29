/* Error-path fixture that IS a well-formed plugin: the first value creator
 * succeeds, the second throws. Exercises the import-time failure path (the
 * plugin's exception propagates to the importer and the partially built
 * value batch is dropped). Registers as `valuethrows`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/valuethrows.<ext> plugin-examples/bad/value_throws.c
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "generic.h"

/* Build an exception of the named builtin class under the EXCEPTION status;
 * forward any non-OK host result unchanged. */
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

static FfiReturn make_seven(const HostApi *host) {
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = host->int_new(host->ctx, 7)};
    return ret;
}

static FfiReturn boom(const HostApi *host) {
    return throw_new(host, "ValueError", "value creation failed");
}

static const ValueDesc VALUES[] = {
    {.name = {.ptr = (const uint8_t *)"seven", .len = 5}, .fun = make_seven},
    {.name = {.ptr = (const uint8_t *)"boom", .len = 4}, .fun = boom},
};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .values = VALUES,
    .values_len = 2,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

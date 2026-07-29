/* Error-path fixture: a module whose only value export has a null name
 * pointer. The loader must reject it with an ImportError, not read through
 * the null. Registers as `badvaluename`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/badvaluename.<ext> plugin-examples/bad/bad_value_name.c
 */
#include <stddef.h>

#include "generic.h"

static FfiReturn make_nil(const HostApi *host) {
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = host->nil_new(host->ctx)};
    return ret;
}

static const ValueDesc VALUES[] = {{
    .name = {.ptr = NULL, .len = 0},
    .fun = make_nil,
}};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .values = VALUES,
    .values_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

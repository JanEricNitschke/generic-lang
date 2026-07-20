/* Error-path fixture: a module whose only export has a null name pointer.
 * The loader must reject it with an ImportError, not read through the null.
 * Registers as `badname`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/badname.<ext> plugin-examples/bad/bad_name.c
 */
#include <stddef.h>
#include <stdint.h>

#include "generic.h"

static FfiReturn noop(const HostApi *host, const GenericValue *args, size_t nargs) {
    (void)args;
    (void)nargs;
    FfiReturn ret = {.status = GENERIC_FFI_STATUS_OK, .value = host->nil_new(host->ctx)};
    return ret;
}

static const uint8_t ARITIES[] = {0};
static const FunctionDesc FUNCTIONS[] = {{
    .name = {.ptr = NULL, .len = 0},
    .arities = ARITIES,
    .arities_len = 1,
    .fun = noop,
}};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = FUNCTIONS,
    .functions_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

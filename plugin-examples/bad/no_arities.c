/* Error-path fixture: a module whose only export declares no arities (null
 * array, zero length). The loader must reject it with an ImportError naming
 * the function. Registers as `noarities`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/noarities.<ext> plugin-examples/bad/no_arities.c
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

static const FunctionDesc FUNCTIONS[] = {{
    .name = {.ptr = (const uint8_t *)"broken", .len = 6},
    .arities = NULL,
    .arities_len = 0,
    .fun = noop,
}};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = FUNCTIONS,
    .functions_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

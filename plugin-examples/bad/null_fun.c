/* Error-path fixture: a module whose only export is a zero-initialized
 * descriptor — its `fun` pointer is null. The loader must reject it with an
 * ImportError naming the function, not call into nothing. Registers as
 * `nullfun`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/nullfun.<ext> plugin-examples/bad/null_fun.c
 */
#include <stddef.h>
#include <stdint.h>

#include "generic.h"

static const uint8_t ARITIES[] = {0};
static const FunctionDesc FUNCTIONS[] = {{
    .name = {.ptr = (const uint8_t *)"broken", .len = 6},
    .arities = ARITIES,
    .arities_len = 1,
    .fun = NULL,
}};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = FUNCTIONS,
    .functions_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

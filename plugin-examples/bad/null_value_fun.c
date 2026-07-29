/* Error-path fixture: a module whose only value export has a null creator
 * pointer. The loader must reject it with an ImportError, not call through
 * the null. Registers as `nullvaluefun`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/nullvaluefun.<ext> plugin-examples/bad/null_value_fun.c
 */
#include <stddef.h>
#include <string.h>

#include "generic.h"

static const ValueDesc VALUES[] = {{
    .name = {.ptr = (const uint8_t *)"broken", .len = 6},
    .fun = NULL,
}};
static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .values = VALUES,
    .values_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

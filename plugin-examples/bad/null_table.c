/* Error-path fixture: a descriptor declaring one function but a NULL
 * function table. The loader must reject it with an ImportError, not read
 * function descriptors through the null. Registers as `nulltable`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/nulltable.<ext> plugin-examples/bad/null_table.c
 */
#include <stddef.h>

#include "generic.h"

static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .functions = NULL,
    .functions_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

/* Error-path fixture: a descriptor declaring one value but a NULL value
 * table. The loader must reject it with an ImportError, not read value
 * descriptors through the null. Registers as `nullvaluetable`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/nullvaluetable.<ext> plugin-examples/bad/null_value_table.c
 */
#include <stddef.h>

#include "generic.h"

static const ModuleDesc DESC = {
    .abi_version = GENERIC_PLUGIN_ABI_VERSION,
    .values = NULL,
    .values_len = 1,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

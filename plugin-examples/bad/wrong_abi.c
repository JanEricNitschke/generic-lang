/* Error-path fixture: a well-formed module that declares an incompatible ABI
 * version. The loader must reject it with an ImportError naming both the
 * expected and the found version — not load it. Registers as `wrongabi`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/wrongabi.<ext> plugin-examples/bad/wrong_abi.c
 */
#include <stddef.h>
#include <stdint.h>

#include "generic.h"

static const FunctionDesc FUNCTIONS[0];
static const ModuleDesc DESC = {
    .abi_version = 9999, /* deliberately incompatible */
    .functions = FUNCTIONS,
    .functions_len = 0,
};

const ModuleDesc *generic_plugin_init(void) { return &DESC; }

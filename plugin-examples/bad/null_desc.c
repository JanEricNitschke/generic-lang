/* Error-path fixture: `generic_plugin_init` returns NULL instead of a
 * module descriptor. The loader must reject it with an ImportError, not
 * dereference the null. Registers as `nulldesc`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/nulldesc.<ext> plugin-examples/bad/null_desc.c
 */
#include <stddef.h>

#include "generic.h"

const ModuleDesc *generic_plugin_init(void) { return NULL; }

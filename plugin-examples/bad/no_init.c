/* Error-path fixture: a valid shared library that does NOT export
 * `generic_plugin_init`. The loader must reject it with an ImportError rather
 * than crash. Registers (is imported) as `noinit`.
 *
 *   cc -shared -fPIC -I crates/generic-lang-api/include \
 *      -o test/plugin/lang/noinit.<ext> plugin-examples/bad/no_init.c
 */
int unrelated_symbol(void) { return 42; }

// Tiny C++ test plugin — proves the plugin ABI is usable from C++ against the
// same generated `generic.h`. Registers under the module name
// `cpp_demo_plugin`. Built by the Makefile `plugin-lang-fixture` step when a
// C++ compiler is present:
//
//   c++ -shared -fPIC -std=c++23 -I crates/generic-lang-api/include
//       -o test/plugin/lang/cpp_demo_plugin.<ext> plugin-examples/cpp/cpp_demo_plugin.cpp
//
// C++11 is the oldest supported standard (`guarded` needs lambdas; the
// descriptor structs use positional aggregate initialization —
// C-style designated initializers would demand C++20). CI compiles this
// file at C++11 via `make plugin-std-check`.
//
// Two things a C++ author must get right and this fixture demonstrates:
//   * `generic_plugin_init` stays `extern "C"` (the header guards it).
//   * a C++ exception must NEVER unwind through an exported function (that is
//     undefined behavior across the C ABI frame). Every body runs inside
//     `guarded`, which turns any `std::exception` into a generic Exception —
//     the C++ analogue of Rust's `catch_unwind`.

#include <cctype>
#include <cstdint>
#include <cstring>
#include <exception>
#include <functional>
#include <stdexcept>
#include <string>

#include "generic.h"

namespace {

FfiReturn ok(GenericValue value) { return FfiReturn{GENERIC_FFI_STATUS_OK, value}; }

FfiStr as_ffi(const std::string &s) {
    return FfiStr{reinterpret_cast<const uint8_t *>(s.data()), s.size()};
}

// Build an exception instance of the named builtin class and return it under
// the EXCEPTION status. Each host call can itself fail (EXCEPTION or FATAL);
// forward any non-OK FfiReturn unchanged, immediately — never relabel or
// swallow it.
FfiReturn throw_new(const HostApi *host, const char *class_name, const std::string &msg) {
    FfiStr name{reinterpret_cast<const uint8_t *>(class_name), std::strlen(class_name)};
    FfiReturn cls = host->builtin_get(host->ctx, name);
    if (cls.status != GENERIC_FFI_STATUS_OK) {
        return cls;
    }
    FfiReturn exc = host->exception_new(host->ctx, cls.value, as_ffi(msg));
    if (exc.status != GENERIC_FFI_STATUS_OK) {
        return exc;
    }
    return FfiReturn{GENERIC_FFI_STATUS_EXCEPTION, exc.value};
}

// Run a plugin body, converting any escaping C++ exception into a generic one
// so nothing unwinds across the C ABI boundary.
FfiReturn guarded(const HostApi *host, const std::function<FfiReturn()> &body) {
    try {
        return body();
    } catch (const std::exception &e) {
        return throw_new(host, "Exception", std::string("cpp_demo_plugin: ") + e.what());
    } catch (...) {
        return throw_new(host, "Exception", "cpp_demo_plugin: unknown C++ exception");
    }
}

// add(a, b) — integer addition.
FfiReturn add(const HostApi *host, const GenericValue *args, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        int64_t a = 0, b = 0;
        if (!host->int_get(host->ctx, args[0], &a) || !host->int_get(host->ctx, args[1], &b)) {
            return throw_new(host, "TypeError", "cpp_demo_plugin.add expects two ints");
        }
        return ok(host->int_new(host->ctx, a + b));
    });
}

// shout(s) — ASCII-uppercase a string and append '!'.
FfiReturn shout(const HostApi *host, const GenericValue *args, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        FfiStr s{};
        if (!host->string_get(host->ctx, args[0], &s)) {
            return throw_new(host, "TypeError", "cpp_demo_plugin.shout expects a string");
        }
        std::string out(reinterpret_cast<const char *>(s.ptr), s.len);
        for (char &c : out) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }
        out.push_back('!');
        FfiReturn made = host->string_new(host->ctx, as_ffi(out));
        if (made.status != GENERIC_FFI_STATUS_OK) {
            return made;
        }
        return ok(made.value);
    });
}

// sum(list) — iterate a generic list through the vtable and sum its ints.
FfiReturn sum(const HostApi *host, const GenericValue *args, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        size_t len = 0;
        if (!host->list_len(host->ctx, args[0], &len)) {
            return throw_new(host, "TypeError", "cpp_demo_plugin.sum expects a list");
        }
        int64_t total = 0;
        for (size_t i = 0; i < len; i++) {
            FfiReturn item = host->list_get(host->ctx, args[0], i);
            if (item.status != GENERIC_FFI_STATUS_OK) {
                return item;  // propagate an IndexError etc.
            }
            int64_t n = 0;
            if (!host->int_get(host->ctx, item.value, &n)) {
                return throw_new(host, "TypeError", "cpp_demo_plugin.sum expects a list of ints");
            }
            total += n;
        }
        return ok(host->int_new(host->ctx, total));
    });
}

// boom() — throw a C++ exception; guarded turns it into a generic Exception
// instead of aborting the process.
FfiReturn boom(const HostApi *host, const GenericValue *, size_t) {
    return guarded(host,
                   [&]() -> FfiReturn { throw std::runtime_error("intentional C++ exception"); });
}

const uint8_t ARITY_1[] = {1};
const uint8_t ARITY_2[] = {2};
const uint8_t ARITY_0[] = {0};
const FunctionDesc FUNCTIONS[] = {
    {FfiStr{reinterpret_cast<const uint8_t *>("add"), 3}, ARITY_2, 1, add},
    {FfiStr{reinterpret_cast<const uint8_t *>("shout"), 5}, ARITY_1, 1, shout},
    {FfiStr{reinterpret_cast<const uint8_t *>("sum"), 3}, ARITY_1, 1, sum},
    {FfiStr{reinterpret_cast<const uint8_t *>("boom"), 4}, ARITY_0, 1, boom},
};
const ModuleDesc DESC = {
    GENERIC_PLUGIN_ABI_VERSION,
    FUNCTIONS,
    sizeof FUNCTIONS / sizeof FUNCTIONS[0],
};

}  // namespace

extern "C" const ModuleDesc *generic_plugin_init(void) { return &DESC; }

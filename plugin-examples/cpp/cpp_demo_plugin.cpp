// Tiny C++ test plugin - proves the plugin ABI is usable from C++ against the
// same generated `generic.h`. Registers under the module name
// `cpp_demo_plugin`. Built by the Makefile `plugin-lang-fixture` step when a
// C++ compiler is present:
//
//   c++ -shared -fPIC -std=c++23 -I crates/generic-lang-api/include
//       -o test/plugin/lang/cpp_demo_plugin.<ext> plugin-examples/cpp/cpp_demo_plugin.cpp
//
// C++11 is the oldest supported standard (`guarded` needs lambdas; the
// descriptor structs use positional aggregate initialization -
// C-style designated initializers would demand C++20). CI compiles this
// file at C++11 via `make plugin-std-check`.
//
// Two things a C++ author must get right and this fixture demonstrates:
//   * `generic_plugin_init` stays `extern "C"` (the header guards it).
//   * a C++ exception must NEVER unwind through an exported function (that is
//     undefined behavior across the C ABI frame). Every body runs inside
//     `guarded`, which turns any `std::exception` into a generic Exception -
//     the C++ analogue of Rust's `catch_unwind`.

#include <cctype>
#include <cstdint>
#include <cstring>
#include <exception>
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
// forward any non-OK FfiReturn unchanged, immediately - never relabel or
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

// A typed error `guarded` translates back into a specific generic exception.
struct PluginThrow {
    const char *class_name;
    std::string message;
};

// Run a plugin body, converting any escaping C++ exception into a generic one
// so nothing unwinds across the C ABI boundary. `body` is taken by forwarding
// reference and invoked in place: constructing a std::function here could
// itself throw (bad_alloc) outside the try, defeating the guard.
template <typename Body>
FfiReturn guarded(const HostApi *host, Body &&body) {
    try {
        return body();
    } catch (const PluginThrow &e) {
        return throw_new(host, e.class_name, e.message);
    } catch (const std::exception &e) {
        return throw_new(host, "Exception", std::string("cpp_demo_plugin: ") + e.what());
    } catch (...) {
        return throw_new(host, "Exception", "cpp_demo_plugin: unknown C++ exception");
    }
}

// add(a, b) - integer addition.
FfiReturn add(const HostApi *host, const GenericValue *args, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        int64_t a = 0, b = 0;
        if (!host->int_get(host->ctx, args[0], &a) || !host->int_get(host->ctx, args[1], &b)) {
            return throw_new(host, "TypeError", "cpp_demo_plugin.add expects two ints");
        }
        return ok(host->int_new(host->ctx, a + b));
    });
}

// shout(s) - ASCII-uppercase a string and append '!'.
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

// sum(list) - iterate a generic list through the vtable and sum its ints.
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

// boom() - throw a C++ exception; guarded turns it into a generic Exception
// instead of aborting the process.
FfiReturn boom(const HostApi *host, const GenericValue *, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        throw std::runtime_error("intentional C++ exception");
    });
}

// --- a plugin class ------------------------------------------------------
//
// `Counter` mirrors the Rust and C examples: hidden native data (count), a
// managed GenericValue (label) reported to the GC via counter_traverse, and a
// generic-side attribute (note/origin) set via attr_set.

// Methods take the receiver as a separate parameter; `args` are the remaining
// arguments only, and arities exclude the receiver.

struct CounterState {
    int64_t count;
    GenericValue label;
};

// drop/traverse are called directly by the host (not via `guarded`); they must
// never let a C++ exception escape. Neither does anything that can throw.
void counter_drop(void *opaque) { delete static_cast<CounterState *>(opaque); }

int32_t counter_traverse(void *opaque, PluginVisitFn visit, void *visit_ctx) {
    if (opaque != nullptr) {
        visit(visit_ctx, static_cast<CounterState *>(opaque)->label);
    }
    return 0;
}

// Validate and return the hidden state as a reference; throws (caught by
// `guarded`) if the instance was never initialized, so callers never null-check.
CounterState &counter_state(const HostApi *host, GenericValue receiver) {
    auto *s = static_cast<CounterState *>(host->instance_get_opaque(host->ctx, receiver));
    if (s == nullptr) {
        throw PluginThrow{"TypeError", "Counter method on uninitialized instance"};
    }
    return *s;
}

FfiReturn counter_init(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t
) {
    return guarded(host, [&]() -> FfiReturn {
        if (host->value_kind(host->ctx, args[0]) == GENERIC_VALUE_KIND_NIL) {
            return throw_new(host, "TypeError", "Counter label must not be nil");
        }
        auto *s = new CounterState{0, args[0]};
        FfiReturn set = host->instance_set_opaque(host->ctx, receiver, s);
        if (set.status != GENERIC_FFI_STATUS_OK) {
            delete s;
            return set;
        }
        FfiReturn origin = host->string_new(host->ctx, as_ffi(std::string("counter")));
        if (origin.status != GENERIC_FFI_STATUS_OK) {
            return origin;
        }
        FfiStr name{reinterpret_cast<const uint8_t *>("origin"), 6};
        FfiReturn aset = host->attr_set(host->ctx, receiver, name, origin.value);
        if (aset.status != GENERIC_FFI_STATUS_OK) {
            return aset;
        }
        return ok(receiver);  // like every __init__, return the receiver
    });
}

FfiReturn counter_increment(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *,
    size_t
) {
    return guarded(host, [&]() -> FfiReturn {
        CounterState &s = counter_state(host, receiver);
        s.count += 1;
        return ok(host->int_new(host->ctx, s.count));
    });
}

FfiReturn counter_value(const HostApi *host, GenericValue receiver, const GenericValue *, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        CounterState &s = counter_state(host, receiver);
        return ok(host->int_new(host->ctx, s.count));
    });
}

FfiReturn counter_label(const HostApi *host, GenericValue receiver, const GenericValue *, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        CounterState &s = counter_state(host, receiver);
        return ok(s.label);
    });
}

FfiReturn counter_set_note(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t
) {
    return guarded(host, [&]() -> FfiReturn {
        FfiStr name{reinterpret_cast<const uint8_t *>("note"), 4};
        FfiReturn set = host->attr_set(host->ctx, receiver, name, args[0]);
        if (set.status != GENERIC_FFI_STATUS_OK) {
            return set;
        }
        return ok(host->nil_new(host->ctx));
    });
}

FfiReturn counter_get_note(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *,
    size_t
) {
    return guarded(host, [&]() -> FfiReturn {
        FfiStr name{reinterpret_cast<const uint8_t *>("note"), 4};
        return host->attr_get(host->ctx, receiver, name);
    });
}

// Returns a new Counter holding the sum; `other` is type-checked against the
// receiver's class before its opaque state is read (no type confusion).
FfiReturn counter_add(
    const HostApi *host,
    GenericValue receiver,
    const GenericValue *args,
    size_t
) {
    return guarded(host, [&]() -> FfiReturn {
        GenericValue other = args[0];
        FfiReturn cls = host->class_of(host->ctx, receiver);
        if (cls.status != GENERIC_FFI_STATUS_OK) {
            return cls;
        }
        FfiReturn is = host->is_instance(host->ctx, other, cls.value);
        if (is.status != GENERIC_FFI_STATUS_OK) {
            return is;
        }
        bool is_counter = false;
        if (!host->bool_get(host->ctx, is.value, &is_counter) || !is_counter) {
            return throw_new(host, "TypeError", "Counter.__add__ expects another Counter");
        }
        CounterState &a = counter_state(host, receiver);
        CounterState &b = counter_state(host, other);
        int64_t sum = a.count + b.count;
        GenericValue label = a.label;  // read before re-entering call_value
        FfiReturn made = host->call_value(host->ctx, cls.value, &label, 1);
        if (made.status != GENERIC_FFI_STATUS_OK) {
            return made;
        }
        counter_state(host, made.value).count = sum;
        return ok(made.value);
    });
}

// A second plugin class with a distinct opaque type, for the cross-class test.
void ticket_drop(void *opaque) { delete static_cast<int64_t *>(opaque); }

FfiReturn ticket_init(const HostApi *host, GenericValue receiver, const GenericValue *, size_t) {
    return guarded(host, [&]() -> FfiReturn {
        auto *id = new int64_t(0);
        FfiReturn set = host->instance_set_opaque(host->ctx, receiver, id);
        if (set.status != GENERIC_FFI_STATUS_OK) {
            delete id;
            return set;
        }
        return ok(receiver);
    });
}

const uint8_t ARITY_1[] = {1};
const uint8_t ARITY_2[] = {2};
const uint8_t ARITY_0[] = {0};

const MethodDesc COUNTER_METHODS[] = {
    {FfiStr{reinterpret_cast<const uint8_t *>("__init__"), 8}, ARITY_1, 1, counter_init},
    {FfiStr{reinterpret_cast<const uint8_t *>("increment"), 9}, ARITY_0, 1, counter_increment},
    {FfiStr{reinterpret_cast<const uint8_t *>("value"), 5}, ARITY_0, 1, counter_value},
    {FfiStr{reinterpret_cast<const uint8_t *>("label"), 5}, ARITY_0, 1, counter_label},
    {FfiStr{reinterpret_cast<const uint8_t *>("set_note"), 8}, ARITY_1, 1, counter_set_note},
    {FfiStr{reinterpret_cast<const uint8_t *>("get_note"), 8}, ARITY_0, 1, counter_get_note},
    {FfiStr{reinterpret_cast<const uint8_t *>("__add__"), 7}, ARITY_1, 1, counter_add},
};

const MethodDesc TICKET_METHODS[] = {
    {FfiStr{reinterpret_cast<const uint8_t *>("__init__"), 8}, ARITY_0, 1, ticket_init},
};

const ClassDesc CLASSES[] = {
    {FfiStr{reinterpret_cast<const uint8_t *>("Counter"), 7},
     COUNTER_METHODS,
     sizeof COUNTER_METHODS / sizeof COUNTER_METHODS[0],
     counter_drop,
     counter_traverse},
    {FfiStr{reinterpret_cast<const uint8_t *>("Ticket"), 6},
     TICKET_METHODS,
     sizeof TICKET_METHODS / sizeof TICKET_METHODS[0],
     ticket_drop,
     nullptr},
};

// tau - a module constant, built once when the module is imported.
FfiReturn make_tau(const HostApi *host) {
    return ok(host->float_new(host->ctx, 6.283185307179586));
}

const ValueDesc VALUES[] = {
    {FfiStr{reinterpret_cast<const uint8_t *>("tau"), 3}, make_tau},
};

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
    CLASSES,
    sizeof CLASSES / sizeof CLASSES[0],
    VALUES,
    sizeof VALUES / sizeof VALUES[0],
};

}  // namespace

extern "C" const ModuleDesc *generic_plugin_init(void) { return &DESC; }

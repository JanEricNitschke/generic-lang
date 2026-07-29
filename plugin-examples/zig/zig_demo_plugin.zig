// Tiny Zig test plugin - proves the plugin ABI is usable from Zig, consuming
// the generated `generic.h` through the build system's translate-c step
// (see build.zig).
// Registers under the module name `zig_demo_plugin`. Built by the Makefile
// `plugin-lang-fixture` step when a Zig toolchain is present:
//
//   cd plugin-examples/zig && zig build -Doptimize=ReleaseSafe

const std = @import("std");
const c = @import("generic_h");

fn ok(value: c.GenericValue) c.FfiReturn {
    return c.FfiReturn{ .status = c.GENERIC_FFI_STATUS_OK, .value = value };
}

// Build an exception instance of the named builtin class and return it under
// the EXCEPTION status. Each host call can itself fail (EXCEPTION or FATAL);
// forward any non-OK FfiReturn unchanged, immediately - never relabel or
// swallow it.
fn throwNew(host: [*c]const c.HostApi, class_name: [*c]const u8, msg: [*c]const u8) c.FfiReturn {
    const name = c.FfiStr{ .ptr = class_name, .len = std.mem.len(class_name) };
    const message = c.FfiStr{ .ptr = msg, .len = std.mem.len(msg) };
    const cls = host.*.builtin_get.?(host.*.ctx, name);
    if (cls.status != c.GENERIC_FFI_STATUS_OK) {
        return cls;
    }
    const exc = host.*.exception_new.?(host.*.ctx, cls.value, message);
    if (exc.status != c.GENERIC_FFI_STATUS_OK) {
        return exc;
    }
    return c.FfiReturn{ .status = c.GENERIC_FFI_STATUS_EXCEPTION, .value = exc.value };
}

// add(a, b) - integer addition.
fn add(host: [*c]const c.HostApi, args: [*c]const c.GenericValue, nargs: usize) callconv(.c) c.FfiReturn {
    _ = nargs;
    var a: i64 = 0;
    var b: i64 = 0;
    if (!host.*.int_get.?(host.*.ctx, args[0], &a) or !host.*.int_get.?(host.*.ctx, args[1], &b)) {
        return throwNew(host, "TypeError", "zig_demo_plugin.add expects two ints");
    }
    return ok(host.*.int_new.?(host.*.ctx, a + b));
}

// shout(s) - ASCII-uppercase a string and append '!'.
fn shout(host: [*c]const c.HostApi, args: [*c]const c.GenericValue, nargs: usize) callconv(.c) c.FfiReturn {
    _ = nargs;
    var s: c.FfiStr = undefined;
    if (!host.*.string_get.?(host.*.ctx, args[0], &s)) {
        return throwNew(host, "TypeError", "zig_demo_plugin.shout expects a string");
    }
    var buf: [256]u8 = undefined;
    if (s.len + 1 > buf.len) {
        return throwNew(host, "ValueError", "zig_demo_plugin.shout string too long");
    }
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        buf[i] = std.ascii.toUpper(s.ptr[i]);
    }
    buf[s.len] = '!';
    const out = c.FfiStr{ .ptr = &buf, .len = s.len + 1 };
    return host.*.string_new.?(host.*.ctx, out);
}

// raise(class_name, message) - throw a fresh instance of the named builtin
// exception class. The string_get bytes are only valid until the next
// re-entering callback, so copy both out into NUL-terminated buffers.
fn raise(host: [*c]const c.HostApi, args: [*c]const c.GenericValue, nargs: usize) callconv(.c) c.FfiReturn {
    _ = nargs;
    var class_name: c.FfiStr = undefined;
    var message: c.FfiStr = undefined;
    if (!host.*.string_get.?(host.*.ctx, args[0], &class_name)) {
        return throwNew(host, "TypeError", "zig_demo_plugin.raise expects (class, message) strings");
    }
    var class_buf: [64]u8 = undefined;
    const cn = @min(class_name.len, class_buf.len - 1);
    @memcpy(class_buf[0..cn], class_name.ptr[0..cn]);
    class_buf[cn] = 0;
    if (!host.*.string_get.?(host.*.ctx, args[1], &message)) {
        return throwNew(host, "TypeError", "zig_demo_plugin.raise expects (class, message) strings");
    }
    var msg_buf: [256]u8 = undefined;
    const mn = @min(message.len, msg_buf.len - 1);
    @memcpy(msg_buf[0..mn], message.ptr[0..mn]);
    msg_buf[mn] = 0;
    return throwNew(host, &class_buf, &msg_buf);
}

// --- a plugin class ------------------------------------------------------
//
// `Counter` mirrors the Rust, C, and C++ examples: hidden native data (count),
// a managed GenericValue (label) reported to the GC via counterTraverse, and a
// generic-side attribute (note/origin) set via attr_set. Methods take the
// receiver as a separate parameter; `args` are the remaining arguments only,
// and arities exclude the receiver.

const CounterState = struct {
    count: i64,
    label: c.GenericValue,
};

// libc is linked (see build.zig), so init and drop share the C allocator.
const counter_allocator = std.heap.c_allocator;

const PluginError = error{Uninitialized};

// Validate the opaque pointer once and hand back a guaranteed non-null
// `*CounterState`; method bodies unwrap it with `try` and never see an optional
// or recheck for null.
fn counterState(host: [*c]const c.HostApi, receiver: c.GenericValue) PluginError!*CounterState {
    const p = host.*.instance_get_opaque.?(host.*.ctx, receiver) orelse return error.Uninitialized;
    return @ptrCast(@alignCast(p));
}

const MethodBody = fn ([*c]const c.HostApi, c.GenericValue, [*c]const c.GenericValue, usize) PluginError!c.FfiReturn;
const MethodFn = fn ([*c]const c.HostApi, c.GenericValue, [*c]const c.GenericValue, usize) callconv(.c) c.FfiReturn;

// Adapt a fallible method body to the C-ABI method signature, turning a
// propagated PluginError into a matching generic exception. The Zig analogue
// of the C++ `guarded` helper: bodies unwrap `counterState` with `try`.
// A body that wants a different message for the same failure uses an
// explicit `catch` (see counterAdd).
fn method(comptime body: MethodBody) MethodFn {
    return struct {
        fn call(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) callconv(.c) c.FfiReturn {
            return body(host, receiver, args, nargs) catch |err| switch (err) {
                error.Uninitialized => throwNew(host, "TypeError", "Counter method on an uninitialized instance"),
            };
        }
    }.call;
}

// Free the opaque state when a Counter (or subclass) is garbage-collected.
fn counterDrop(opaque_ptr: ?*anyopaque) callconv(.c) void {
    if (opaque_ptr) |p| {
        const s: *CounterState = @ptrCast(@alignCast(p));
        counter_allocator.destroy(s);
    }
}

// Report the held label so the GC keeps it alive while the Counter lives.
fn counterTraverse(opaque_ptr: ?*anyopaque, visit: c.PluginVisitFn, visit_ctx: ?*anyopaque) callconv(.c) i32 {
    if (opaque_ptr) |p| {
        const s: *CounterState = @ptrCast(@alignCast(p));
        visit.?(visit_ctx, s.label);
    }
    return 0;
}

fn counterInit(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = nargs;
    if (host.*.value_kind.?(host.*.ctx, args[0]) == c.GENERIC_VALUE_KIND_NIL) {
        return throwNew(host, "TypeError", "Counter label must not be nil");
    }
    const s = counter_allocator.create(CounterState) catch {
        return throwNew(host, "Exception", "Counter.__init__ out of memory");
    };
    s.* = .{ .count = 0, .label = args[0] };
    const set = host.*.instance_set_opaque.?(host.*.ctx, receiver, s);
    if (set.status != c.GENERIC_FFI_STATUS_OK) {
        counter_allocator.destroy(s);
        return set;
    }
    const origin = host.*.string_new.?(host.*.ctx, .{ .ptr = "counter", .len = 7 });
    if (origin.status != c.GENERIC_FFI_STATUS_OK) return origin;
    const aset = host.*.attr_set.?(host.*.ctx, receiver, .{ .ptr = "origin", .len = 6 }, origin.value);
    if (aset.status != c.GENERIC_FFI_STATUS_OK) return aset;
    return ok(receiver); // like every __init__, return the receiver
}

fn counterIncrement(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = args;
    _ = nargs;
    const s = try counterState(host, receiver);
    s.count += 1;
    return ok(host.*.int_new.?(host.*.ctx, s.count));
}

fn counterValue(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = args;
    _ = nargs;
    const s = try counterState(host, receiver);
    return ok(host.*.int_new.?(host.*.ctx, s.count));
}

fn counterLabel(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = args;
    _ = nargs;
    const s = try counterState(host, receiver);
    return ok(s.label);
}

fn counterSetNote(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = nargs;
    const set = host.*.attr_set.?(host.*.ctx, receiver, .{ .ptr = "note", .len = 4 }, args[0]);
    if (set.status != c.GENERIC_FFI_STATUS_OK) return set;
    return ok(host.*.nil_new.?(host.*.ctx));
}

fn counterGetNote(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = args;
    _ = nargs;
    return host.*.attr_get.?(host.*.ctx, receiver, .{ .ptr = "note", .len = 4 });
}

// Returns a new Counter holding the sum; `other` is type-checked against the
// receiver's class before its opaque state is read (no type confusion).
fn counterAdd(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = nargs;
    const other = args[0];
    const cls = host.*.class_of.?(host.*.ctx, receiver);
    if (cls.status != c.GENERIC_FFI_STATUS_OK) return cls;
    const is = host.*.is_instance.?(host.*.ctx, other, cls.value);
    if (is.status != c.GENERIC_FFI_STATUS_OK) return is;
    var is_counter: bool = false;
    if (!host.*.bool_get.?(host.*.ctx, is.value, &is_counter) or !is_counter) {
        return throwNew(host, "TypeError", "Counter.__add__ expects another Counter");
    }
    const a = try counterState(host, receiver);
    const b = try counterState(host, other);
    const sum = a.count + b.count;
    const label = a.label; // read before re-entering call_value
    const made = host.*.call_value.?(host.*.ctx, cls.value, &label, 1);
    if (made.status != c.GENERIC_FFI_STATUS_OK) return made;
    const n = counterState(host, made.value) catch
        return throwNew(host, "Exception", "Counter.__add__ construction failed");
    n.count = sum;
    return ok(made.value);
}

// A second plugin class with a distinct opaque type, for the cross-class test.
const TicketState = struct { id: i64 };

fn ticketDrop(opaque_ptr: ?*anyopaque) callconv(.c) void {
    if (opaque_ptr) |p| {
        const t: *TicketState = @ptrCast(@alignCast(p));
        counter_allocator.destroy(t);
    }
}

fn ticketInit(host: [*c]const c.HostApi, receiver: c.GenericValue, args: [*c]const c.GenericValue, nargs: usize) PluginError!c.FfiReturn {
    _ = args;
    _ = nargs;
    const t = counter_allocator.create(TicketState) catch {
        return throwNew(host, "Exception", "Ticket.__init__ out of memory");
    };
    t.* = .{ .id = 0 };
    const set = host.*.instance_set_opaque.?(host.*.ctx, receiver, t);
    if (set.status != c.GENERIC_FFI_STATUS_OK) {
        counter_allocator.destroy(t);
        return set;
    }
    return ok(receiver);
}

const ARITY_0 = [_]u8{0};
const ARITY_1 = [_]u8{1};
const ARITY_2 = [_]u8{2};

const COUNTER_METHODS = [_]c.MethodDesc{
    .{ .name = .{ .ptr = "__init__", .len = 8 }, .arities = &ARITY_1, .arities_len = 1, .fun = method(counterInit) },
    .{ .name = .{ .ptr = "increment", .len = 9 }, .arities = &ARITY_0, .arities_len = 1, .fun = method(counterIncrement) },
    .{ .name = .{ .ptr = "value", .len = 5 }, .arities = &ARITY_0, .arities_len = 1, .fun = method(counterValue) },
    .{ .name = .{ .ptr = "label", .len = 5 }, .arities = &ARITY_0, .arities_len = 1, .fun = method(counterLabel) },
    .{ .name = .{ .ptr = "set_note", .len = 8 }, .arities = &ARITY_1, .arities_len = 1, .fun = method(counterSetNote) },
    .{ .name = .{ .ptr = "get_note", .len = 8 }, .arities = &ARITY_0, .arities_len = 1, .fun = method(counterGetNote) },
    .{ .name = .{ .ptr = "__add__", .len = 7 }, .arities = &ARITY_1, .arities_len = 1, .fun = method(counterAdd) },
};

const TICKET_METHODS = [_]c.MethodDesc{
    .{ .name = .{ .ptr = "__init__", .len = 8 }, .arities = &ARITY_0, .arities_len = 1, .fun = method(ticketInit) },
};

const CLASSES = [_]c.ClassDesc{
    .{ .name = .{ .ptr = "Counter", .len = 7 }, .methods = &COUNTER_METHODS, .methods_len = COUNTER_METHODS.len, .drop = counterDrop, .traverse = counterTraverse },
    .{ .name = .{ .ptr = "Ticket", .len = 6 }, .methods = &TICKET_METHODS, .methods_len = TICKET_METHODS.len, .drop = ticketDrop, .traverse = null },
};

const FUNCTIONS = [_]c.FunctionDesc{
    .{ .name = .{ .ptr = "add", .len = 3 }, .arities = &ARITY_2, .arities_len = 1, .fun = add },
    .{ .name = .{ .ptr = "shout", .len = 5 }, .arities = &ARITY_1, .arities_len = 1, .fun = shout },
    .{ .name = .{ .ptr = "raise", .len = 5 }, .arities = &ARITY_2, .arities_len = 1, .fun = raise },
};

// golden_ratio - a module constant, built once when the module is imported.
fn makeGoldenRatio(host: [*c]const c.HostApi) callconv(.c) c.FfiReturn {
    return ok(host.*.float_new.?(host.*.ctx, 1.618033988749895));
}

const VALUES = [_]c.ValueDesc{
    .{ .name = .{ .ptr = "golden_ratio", .len = 12 }, .fun = makeGoldenRatio },
};

const DESC = c.ModuleDesc{
    .abi_version = c.GENERIC_PLUGIN_ABI_VERSION,
    .functions = &FUNCTIONS,
    .functions_len = FUNCTIONS.len,
    .classes = &CLASSES,
    .classes_len = CLASSES.len,
    .values = &VALUES,
    .values_len = VALUES.len,
};

export fn generic_plugin_init() callconv(.c) [*c]const c.ModuleDesc {
    return &DESC;
}

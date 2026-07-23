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

const ARITY_1 = [_]u8{1};
const ARITY_2 = [_]u8{2};

const FUNCTIONS = [_]c.FunctionDesc{
    .{ .name = .{ .ptr = "add", .len = 3 }, .arities = &ARITY_2, .arities_len = 1, .fun = add },
    .{ .name = .{ .ptr = "shout", .len = 5 }, .arities = &ARITY_1, .arities_len = 1, .fun = shout },
    .{ .name = .{ .ptr = "raise", .len = 5 }, .arities = &ARITY_2, .arities_len = 1, .fun = raise },
};

const DESC = c.ModuleDesc{
    .abi_version = c.GENERIC_PLUGIN_ABI_VERSION,
    .functions = &FUNCTIONS,
    .functions_len = FUNCTIONS.len,
};

export fn generic_plugin_init() callconv(.c) [*c]const c.ModuleDesc {
    return &DESC;
}

//! Build script for the Zig example plugin.
//!
//! C interop goes through the build system's translate-c step:
//! the generated plugin header is translated into a regular module that
//! `zig_demo_plugin.zig` imports as `generic_h`.

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const generic_h = b.addTranslateC(.{
        .root_source_file = b.path("../../crates/generic-lang-api/include/generic.h"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const mod = b.createModule(.{
        .root_source_file = b.path("zig_demo_plugin.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addImport("generic_h", generic_h.createModule());

    const lib = b.addLibrary(.{
        .name = "zig_demo_plugin",
        .root_module = mod,
        .linkage = .dynamic,
    });
    b.installArtifact(lib);
}

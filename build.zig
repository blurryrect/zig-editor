const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard target options allow the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    // This creates a module, which represents a collection of source files alongside
    // some compilation options, such as optimization mode and linked system libraries.
    // This module can be imported by consumers of this package.
    const pleditor_module = b.addModule("pleditor", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Create the executable
    const exe = b.addExecutable(.{
        .name = "pleditor",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "pleditor", .module = pleditor_module },
            },
        }),
    });

    // Link with libc for terminal and file operations
    exe.linkLibC();

    // This declares intent for the executable to be installed into the
    // install prefix when running `zig build`.
    b.installArtifact(exe);

    // This creates a top level step that can be invoked with `zig build run`.
    const run_step = b.step("run", "Run the pleditor application");

    // This creates a RunArtifact step in the build graph.
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- filename.txt`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Creates a test executable for the pleditor module
    const module_tests = b.addTest(.{
        .root_module = pleditor_module,
    });

    // A run step that will run the module tests
    const run_module_tests = b.addRunArtifact(module_tests);

    // Creates a test executable for the main executable's root module
    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    // A run step that will run the executable tests
    const run_exe_tests = b.addRunArtifact(exe_tests);

    // A top level step for running all tests
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_module_tests.step);
    test_step.dependOn(&run_exe_tests.step);
}

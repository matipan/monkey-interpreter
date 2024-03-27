const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "monkey",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const run_lexer_unit_tests = b.addRunArtifact(b.addTest(.{
        .root_source_file = .{ .path = "src/lexer/lexer.zig" },
        .target = target,
        .optimize = optimize,
        .main_mod_path = .{ .path = "src/" },
    }));

    const run_parser_unit_tests = b.addRunArtifact(b.addTest(.{
        .root_source_file = .{ .path = "src/parser/parser.zig" },
        .target = target,
        .optimize = optimize,
        .main_mod_path = .{ .path = "src/" },
    }));

    b.step("lexer-test", "Run lexer unit tests").dependOn(&run_lexer_unit_tests.step);
    b.step("parser-test", "Run parser unit tests").dependOn(&run_parser_unit_tests.step);
}

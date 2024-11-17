const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const token_mod = b.addModule("token", .{
        .root_source_file = b.path("src/token/index.zig"),
    });

    const ast_mod = b.addModule("ast", .{
        .root_source_file = b.path("src/ast/ast.zig"),
    });
    ast_mod.addImport("token", token_mod);

    const lexer_mod = b.addModule("lexer", .{
        .root_source_file = b.path("src/lexer/lexer.zig"),
    });
    lexer_mod.addImport("token", token_mod);

    const parser_mod = b.addModule("parser", .{
        .root_source_file = b.path("src/parser/parser.zig"),
    });
    parser_mod.addImport("lexer", lexer_mod);
    parser_mod.addImport("token", token_mod);
    parser_mod.addImport("ast", ast_mod);

    const repl_mod = b.addModule("repl", .{
        .root_source_file = b.path("src/repl/repl.zig"),
    });
    repl_mod.addImport("parser", parser_mod);
    repl_mod.addImport("lexer", lexer_mod);
    repl_mod.addImport("token", token_mod);

    const eval_mod = b.addModule("eval", .{
        .root_source_file = b.path("src/eval/eval.zig"),
    });
    eval_mod.addImport("parser", parser_mod);
    eval_mod.addImport("ast", ast_mod);

    const exe = b.addExecutable(.{
        .name = "monkey",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("repl", repl_mod);

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
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const lexer_test_step = b.addTest(.{
        .root_source_file = b.path("src/lexer/lexer.zig"),
        .target = target,
        .optimize = optimize,
    });
    lexer_test_step.root_module.addImport("token", token_mod);
    const run_lexer_unit_tests = b.addRunArtifact(lexer_test_step);

    const parser_test_step = b.addTest(.{
        .root_source_file = b.path("src/parser/parser.zig"),
        .target = target,
        .optimize = optimize,
    });
    parser_test_step.root_module.addImport("ast", ast_mod);
    parser_test_step.root_module.addImport("lexer", lexer_mod);
    parser_test_step.root_module.addImport("token", token_mod);
    const run_parser_unit_tests = b.addRunArtifact(parser_test_step);

    b.step("lexer-test", "Run lexer unit tests").dependOn(&run_lexer_unit_tests.step);
    b.step("parser-test", "Run parser unit tests").dependOn(&run_parser_unit_tests.step);
}

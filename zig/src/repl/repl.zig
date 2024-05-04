const std = @import("std");
const Parser = @import("../parser/parser.zig").Parser;
const Lexer = @import("../lexer/lexer.zig").Lexer;
const tokenType = @import("../token/index.zig").TokenType;

pub fn run() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    while (true) {
        std.debug.print(">> ", .{});

        var buffer: [1024]u8 = undefined;
        const bytes = try stdin.readUntilDelimiterOrEof(buffer[0..], '\n');

        if (bytes) |input| {
            var p = Parser.init(Lexer.init(input));

            const program = try p.parseProgram(alloc);
            defer program.deinit();

            const value = try program.string(alloc);
            defer alloc.free(value);

            std.debug.print("{s}\n", .{value});
        } else {
            std.debug.print("No input received.\n", .{});
        }
    }

    return;
}

const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const tokenType = @import("../token/index.zig").TokenType;

pub fn run() !void {
    const stdin = std.io.getStdIn().reader();

    while (true) {
        std.debug.print(">> ", .{});

        var buffer: [1024]u8 = undefined;
        const bytes = try stdin.readUntilDelimiterOrEof(buffer[0..], '\n');

        var l: lexer.Lexer = undefined;
        if (bytes) |input| {
            l = lexer.Lexer.init(input);
            while (true) {
                const t = l.nextToken();
                if (t.tokenType == tokenType.eof) {
                    break;
                }

                std.debug.print("{any} - {s}\n", .{ t.tokenType, t.literal });
            }
        } else {
            std.debug.print("No input received.\n", .{});
        }
    }

    return;
}

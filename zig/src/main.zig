const std = @import("std");
const repl = @import("repl");

pub fn main() !void {
    try repl.run();
}

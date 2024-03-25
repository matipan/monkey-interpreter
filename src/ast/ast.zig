const std = @import("std");
const Token = @import("../token/token.zig").Token;

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,

    pub fn literal(self: Node) []const u8 {
        switch (self) {
            inline else => |case| return case.literal(),
        }
    }
};

pub const Statement = union(enum) {
    let: LetStatement,
    return_with: ReturnStatement,
    expression: ExpressionStatement,

    pub fn literal(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.literal(),
        }
    }
};

pub const LetStatement = struct {
    name: Identifier,
    value: Expression,

    pub fn literal(self: LetStatement) []const u8 {
        return self.name.token.literal;
    }
};

pub const ReturnStatement = struct {
    identifier: Identifier,

    pub fn literal(self: ReturnStatement) []const u8 {
        return self.identifier.token.literal;
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,

    pub fn literal(self: ExpressionStatement) []const u8 {
        return self.expression.literal();
    }
};

pub const Expression = union(enum) {
    empty: EmptyExpression,
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix: PrefixExpression,

    pub fn literal(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| {
                std.debug.print("token type = {any}\n", .{case.token.tokenType});
                return case.token.literal;
            },
        }
    }
};

pub const EmptyExpression = struct {
    token: Token,
};

pub const Identifier = struct {
    token: Token,
};

pub const IntegerLiteral = struct {
    token: Token,
};

pub const PrefixExpression = struct {
    // The prefix token itself (e.g. !)
    token: Token,
    operator: []const u8,
    right: *const Expression,
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Program {
        return Program{
            .statements = std.ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: Program) void {
        return self.statements.deinit();
    }
};

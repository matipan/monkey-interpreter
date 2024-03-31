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

    pub fn string(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        switch (self) {
            inline else => |case| return case.string(alloc),
        }
    }
};

pub const LetStatement = struct {
    name: Identifier,
    value: Expression,

    pub fn literal(self: LetStatement) []const u8 {
        return self.name.token.literal;
    }

    pub fn string(self: LetStatement, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("let");
        try str.appendSlice(" ");
        try str.appendSlice(self.name.token.literal);
        try str.appendSlice(" = ");
        try str.appendSlice(try self.value.string(alloc));

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);
        return result;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    expression: Expression,

    pub fn literal(self: ReturnStatement) []const u8 {
        return self.expression.literal();
    }

    pub fn string(self: ReturnStatement, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("return ");
        const exp = try self.expression.string(alloc);
        try str.appendSlice(exp);

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);
        return result;
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,

    pub fn literal(self: ExpressionStatement) []const u8 {
        return self.expression.literal();
    }

    pub fn string(self: ExpressionStatement, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        return self.expression.string(alloc);
    }
};

pub const Expression = union(enum) {
    empty: EmptyExpression,
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    boolean_literal: BooleanLiteral,
    prefix: PrefixExpression,
    infix: InfixExpression,

    pub fn literal(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| return case.token.literal,
        }
    }

    pub fn string(self: Expression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        switch (self) {
            inline else => |case| return case.string(alloc),
        }
    }
};

pub const EmptyExpression = struct {
    token: Token,

    pub fn string(self: EmptyExpression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        const result = try alloc.alloc(u8, self.token.literal.len);
        std.mem.copyForwards(u8, result, self.token.literal);
        return result;
    }
};

pub const Identifier = struct {
    token: Token,

    pub fn string(self: Identifier, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        const result = try alloc.alloc(u8, self.token.literal.len);
        std.mem.copyForwards(u8, result, self.token.literal);
        return result;
    }
};

pub const IntegerLiteral = struct {
    token: Token,

    pub fn string(self: IntegerLiteral, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        const result = try alloc.alloc(u8, self.token.literal.len);
        std.mem.copyForwards(u8, result, self.token.literal);
        return result;
    }
};

pub const BooleanLiteral = struct {
    token: Token,

    pub fn string(self: BooleanLiteral, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        const result = try alloc.alloc(u8, self.token.literal.len);
        std.mem.copyForwards(u8, result, self.token.literal);
        return result;
    }
};

pub const PrefixExpression = struct {
    // The prefix token itself (e.g. !)
    token: Token,
    operator: []const u8,
    right: *const Expression,

    pub fn string(self: PrefixExpression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("(");
        try str.appendSlice(self.operator);
        const rightExp = try self.right.string(alloc);
        try str.appendSlice(rightExp);
        try str.appendSlice(")");

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);

        return result;
    }
};

pub const InfixExpression = struct {
    token: Token,
    // The operator token itself (e.g. !)
    operator: []const u8,
    left: *const Expression,
    right: *const Expression,

    pub fn string(self: InfixExpression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("(");
        const leftExp = try self.left.string(alloc);
        try str.appendSlice(leftExp);
        try str.appendSlice(self.operator);
        const rightExp = try self.right.string(alloc);
        try str.appendSlice(rightExp);
        try str.appendSlice(")");

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);

        return result;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    expressions: std.ArrayList(*const Expression),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) Program {
        return Program{
            .statements = std.ArrayList(Statement).init(alloc),
            .expressions = std.ArrayList(*const Expression).init(alloc),
            .alloc = alloc,
        };
    }

    pub fn deinit(self: Program) void {
        // destroy all expressions that were allocated
        for (self.expressions.items) |exp| {
            self.alloc.destroy(exp);
        }

        self.expressions.deinit();
        self.statements.deinit();
    }

    // lets start by simply printing a string for each statement and appending
    // a new line at the end of each
    //
    pub fn string(self: Program, alloc: std.mem.Allocator) ![]u8 {
        var prog = std.ArrayList(u8).init(alloc);
        defer prog.deinit();

        for (self.statements.items) |stmt| {
            const value: []u8 = try stmt.string(alloc);
            try prog.appendSlice(value);
            try prog.append('\n');
        }

        const result = try alloc.alloc(u8, prog.items.len);
        std.mem.copyForwards(u8, result, prog.items);
        return result;
    }
};

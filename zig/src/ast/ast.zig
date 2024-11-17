const std = @import("std");
const Token = @import("token").Token;

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
        const exp = try self.value.string(alloc);
        defer alloc.free(exp);
        try str.appendSlice(exp);

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
        defer alloc.free(exp);

        try str.appendSlice(exp);

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);
        return result;
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(Statement),

    pub fn literal(self: BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: BlockStatement, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        for (self.statements.items) |stmt| {
            // we can defer the freeing of this memory block
            // because we are copying it below
            const value: []u8 = try stmt.string(alloc);
            defer alloc.free(value);

            try str.appendSlice(value);
            try str.append('\n');
        }

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
    if_expression: IfExpression,
    function_literal: FunctionLiteral,
    call_expression: CallExpression,

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
        defer alloc.free(rightExp);
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

        // appendSlice does @memcpy internally so we can free the memory
        // after we've added to the slice.
        const leftExp = try self.left.string(alloc);
        defer alloc.free(leftExp);
        try str.appendSlice(leftExp);
        try str.appendSlice(self.operator);
        const rightExp = try self.right.string(alloc);
        defer alloc.free(rightExp);
        try str.appendSlice(rightExp);
        try str.appendSlice(")");

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);

        return result;
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: BlockStatement,

    pub fn string(self: IfExpression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("if (");
        const cond = try self.condition.string(alloc);
        defer alloc.free(cond);
        try str.appendSlice(cond);
        try str.appendSlice(") ");
        if (self.consequence.statements.items.len == 0) {
            try str.appendSlice("{}");
        } else {
            try str.appendSlice("{\n");
            const cons = try self.consequence.string(alloc);
            defer alloc.free(cons);
            try str.appendSlice(cons);
            try str.appendSlice("}");
        }

        if (self.alternative.statements.items.len == 0) {
            try str.appendSlice(" else {}");
        } else {
            try str.appendSlice(" else {\n");
            const cons = try self.alternative.string(alloc);
            defer alloc.free(cons);
            try str.appendSlice(cons);
            try str.appendSlice("}");
        }

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);

        return result;
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayList(Identifier),
    block: BlockStatement,

    pub fn string(self: FunctionLiteral, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        try str.appendSlice("fn (");
        for (self.parameters.items, 0..) |param, i| {
            // we can defer the freeing of this memory block
            // because we are copying it below
            const value: []u8 = try param.string(alloc);
            defer alloc.free(value);

            try str.appendSlice(value);
            if (i < self.parameters.items.len - 1) {
                try str.append(',');
            }
        }
        try str.appendSlice(") {");
        if (self.block.statements.items.len > 0) {
            try str.appendSlice("\n");
            const block_stmts = try self.block.string(alloc);
            defer alloc.free(block_stmts);
            try str.appendSlice(block_stmts);
        }
        try str.appendSlice("}");

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);
        return result;
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *const Expression,
    arguments: std.ArrayList(*const Expression),

    pub fn string(self: CallExpression, alloc: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        var str = std.ArrayList(u8).init(alloc);
        defer str.deinit();

        const function = try self.function.string(alloc);
        defer alloc.free(function);

        try str.appendSlice(function);
        try str.append('(');
        for (self.arguments.items, 0..) |arg, i| {
            // we can defer the freeing of this memory block
            // because we are copying it below
            const value: []u8 = try arg.string(alloc);
            defer alloc.free(value);

            try str.appendSlice(value);
            if (i < self.arguments.items.len - 1) {
                try str.append(',');
            }
        }
        try str.append(')');

        const result = try alloc.alloc(u8, str.items.len);
        std.mem.copyForwards(u8, result, str.items);
        return result;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    expressions: std.ArrayList(*const Expression),
    arena: std.heap.ArenaAllocator,

    pub fn init(alloc: std.mem.Allocator) Program {
        return Program{
            .statements = std.ArrayList(Statement).init(alloc),
            .expressions = std.ArrayList(*const Expression).init(alloc),
            .arena = std.heap.ArenaAllocator.init(alloc),
        };
    }

    pub fn deinit(self: Program) void {
        self.expressions.deinit();
        self.statements.deinit();
        self.arena.deinit();
    }

    // lets start by simply printing a string for each statement and appending
    // a new line at the end of each
    pub fn string(self: Program, alloc: std.mem.Allocator) ![]u8 {
        var prog = std.ArrayList(u8).init(alloc);
        defer prog.deinit();

        for (self.statements.items) |stmt| {
            // we can defer the freeing of this memory block
            // because we are copying it below
            const value: []u8 = try stmt.string(alloc);
            defer alloc.free(value);

            try prog.appendSlice(value);
            try prog.append('\n');
        }

        const result = try alloc.alloc(u8, prog.items.len);
        std.mem.copyForwards(u8, result, prog.items);
        return result;
    }
};

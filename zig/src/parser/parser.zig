const std = @import("std");
const testing = std.testing;
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Token = @import("../token/index.zig").Token;
const tokenType = @import("../token/index.zig").TokenType;
const ast = @import("../ast/ast.zig");

const Operator = enum(u8) {
    lowest,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,

    pub fn precedence(tokType: tokenType) u8 {
        return switch (tokType) {
            .eq, .notEq => @intFromEnum(Operator.equals),
            .lt, .gt => @intFromEnum(Operator.lessgreater),
            .plus, .minus => @intFromEnum(Operator.sum),
            .slash, .asterisk => @intFromEnum(Operator.product),
            else => @intFromEnum(Operator.lowest),
        };
    }
};

const ParseError = error{
    InternalError,

    // Let errors
    MissingAssign,
    MissingIdentOrLiteral,
};

const Parser = struct {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,

    pub fn init(lexer: Lexer) Parser {
        var p = Parser{
            .lexer = lexer,
            .current_token = Token{ .tokenType = tokenType.eof, .literal = "" },
            .peek_token = Token{ .tokenType = tokenType.eof, .literal = "" },
        };
        // NOTE: we call nextToken twice so that current token and peek token
        // are set to tokens from the program instead of EOF as defined above
        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser, alloc: std.mem.Allocator) ParseError!ast.Program {
        var prog = ast.Program.init(alloc);

        // iterate over the program until we reach the end of it
        while (self.current_token.tokenType != tokenType.eof) {
            // We parse top level tokens in it's own function that is responsible
            // for also advancing the parser and leaving it ready to process the
            // next token (which is why we look at current_token here).
            const stmt = switch (self.current_token.tokenType) {
                .let => try self.parseLetStatement(),
                .returnWith => try self.parseReturnStatement(),
                // receives a program that will be used to allocate expression
                // on the heap that live in the context of the program
                else => try self.parseExpressionStatement(&prog),
            };

            // skip semicolon so that parser is ready for next stmt or expression
            self.nextToken();

            // we only reach here once an actual statement has been initialized
            prog.statements.append(stmt) catch |err| {
                std.debug.print("could not append statements: {any}\n", .{err});
                return ParseError.InternalError;
            };
        }

        return prog;
    }

    fn parseReturnStatement(self: *Parser) ParseError!ast.Statement {
        // TODO: how do we handle expression statements on return

        // if the next token is not an identifier or a literal value
        // then this is an invalid return statement
        switch (self.peek_token.tokenType) {
            .ident, .int, .boolTrue, .boolFalse => self.nextToken(),
            else => return ParseError.MissingIdentOrLiteral,
        }

        const returnStmt = ast.ReturnStatement{
            .identifier = ast.Identifier{ .token = self.current_token },
        };

        // skip until we find a semi colon so that the parsing is ready for
        // the next statement
        while (self.current_token.tokenType != tokenType.semicolon) {
            self.nextToken();
        }

        return ast.Statement{ .return_with = returnStmt };
    }

    fn parseLetStatement(self: *Parser) ParseError!ast.Statement {
        // if the next token is not an identifier then this is an invalid
        // let statement
        if (self.peek_token.tokenType != tokenType.ident) {
            return ParseError.MissingIdentOrLiteral;
        }

        // advance to the next token and save the name of the let statement
        // with the token itself
        self.nextToken();
        var let: ast.LetStatement = undefined;
        let.name = ast.Identifier{ .token = self.current_token };

        // if the next token is not an assign then this is an invalid let
        // statement
        if (self.peek_token.tokenType != tokenType.assign) {
            return ParseError.MissingAssign;
        }

        // TODO: handle expression statements
        // now we just need to store the value of the let statement. We also
        // store the entire token. The value needs to be an expression stmt

        // skip until we find a semi colon so that the parsing is ready for
        // the next statement
        while (self.current_token.tokenType != tokenType.semicolon) {
            self.nextToken();
        }

        return ast.Statement{ .let = let };
    }

    fn parseExpressionStatement(self: *Parser, program: *ast.Program) ParseError!ast.Statement {
        const exp = try self.parseExpression(@intFromEnum(Operator.lowest), program);

        // skip until we find a semi colon so that the parsing is ready for
        // the next statement
        while (self.current_token.tokenType != tokenType.semicolon) {
            self.nextToken();
        }

        return ast.Statement{
            .expression = ast.ExpressionStatement{
                .expression = exp,
            },
        };
    }

    fn prefixParseFn(tokType: tokenType) ParseError!*const fn (*Parser, *ast.Program) ParseError!ast.Expression {
        return switch (tokType) {
            .ident => Parser.parseIdentifier,
            .int => Parser.parseIntegerLiteral,
            .bang, .minus => Parser.parsePrefixExpression,
            else => ParseError.InternalError,
        };
    }

    fn infixParseFn(tokType: tokenType) ParseError!*const fn (*Parser, *ast.Program, ast.Expression) ParseError!ast.Expression {
        return switch (tokType) {
            .plus, .minus, .slash, .asterisk, .eq, .notEq, .lt, .gt => Parser.parseInfixExpression,
            else => ParseError.InternalError,
        };
    }

    fn parseExpression(self: *Parser, precedence: u8, program: *ast.Program) ParseError!ast.Expression {
        const parseFn = try prefixParseFn(self.current_token.tokenType);
        var leftExp = try parseFn(self, program);

        // we exit when we find a semicolon or the precedence of the operator is
        // greater than the precedence of the next token
        while (self.peek_token.tokenType != tokenType.semicolon and self.current_token.tokenType != tokenType.eof and precedence < Operator.precedence(self.peek_token.tokenType)) {
            // if we don't find any we need to return the left expression
            const infixFn = infixParseFn(self.peek_token.tokenType) catch {
                // break of the loop to return leftExp below
                break;
            };

            // move on to the next token, parse the new expression and re-assign
            // it. This is so that more complicated expressions that have many
            // infix operations within can be created properly
            self.nextToken();
            leftExp = try infixFn(self, program, leftExp);
        }

        return leftExp;
    }

    // parseInfixExpression gets called once the `left` expression has already
    // been parsed
    fn parseInfixExpression(self: *Parser, program: *ast.Program, left: ast.Expression) ParseError!ast.Expression {
        const cur_token = self.current_token;
        const prec = Operator.precedence(cur_token.tokenType);

        self.nextToken();
        const right = try self.parseExpression(prec, program);

        // allocate a pointer for the right expression
        const right_ptr = program.alloc.create(ast.Expression) catch |err| {
            std.debug.print("could not allocate expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        program.expressions.append(right_ptr) catch |err| {
            std.debug.print("could not append expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        right_ptr.* = right;

        // allocate a pointer for the left expression
        const left_ptr = program.alloc.create(ast.Expression) catch |err| {
            std.debug.print("could not allocate expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        program.expressions.append(left_ptr) catch |err| {
            std.debug.print("could not append expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        left_ptr.* = left;

        return ast.Expression{
            .infix = ast.InfixExpression{
                .right = right_ptr,
                .left = left_ptr,
                .operator = tokenType.string(cur_token.tokenType),
                .token = cur_token,
            },
        };
    }

    // NOTE: I'm not entirely sure if I'm handling allocations and such the right
    // way. But I want to finish the book and maybe later learn a bit better
    // how we could structure things in a more native zig way. Right now it feels patched
    // to make it work with how the Go version of the interpreter is written
    fn parsePrefixExpression(self: *Parser, program: *ast.Program) ParseError!ast.Expression {
        const operator_token = self.current_token;

        // if we are in here then we've already identified that we are dealing
        // with a prefix expression (e.g. '!5'). So we need to move over to
        // get the next token and obtain the prefixParseFn of that one
        self.nextToken();

        const right = try self.parseExpression(@intFromEnum(Operator.prefix), program);

        const right_ptr = program.alloc.create(ast.Expression) catch |err| {
            std.debug.print("could not allocate expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        program.expressions.append(right_ptr) catch |err| {
            std.debug.print("could not append expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        right_ptr.* = right;

        return ast.Expression{
            .prefix = ast.PrefixExpression{
                .token = operator_token,
                .operator = tokenType.string(operator_token.tokenType),
                .right = right_ptr,
            },
        };
    }

    fn parseIdentifier(self: *Parser, _: *ast.Program) ParseError!ast.Expression {
        return ast.Expression{
            .identifier = ast.Identifier{
                .token = self.current_token,
            },
        };
    }

    fn parseIntegerLiteral(self: *Parser, _: *ast.Program) ParseError!ast.Expression {
        return ast.Expression{
            .integer_literal = ast.IntegerLiteral{
                .token = self.current_token,
            },
        };
    }
};

test "Parser.parseProgram" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const code =
        \\let five = 5;
        \\return five;
        \\return 5;
        \\return true;
        \\return false;
    ;
    var p = Parser.init(Lexer.init(code));
    var program = try p.parseProgram(allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "five",
        "five",
        "5",
        tokenType.boolTrue.string(),
        tokenType.boolFalse.string(),
    };

    testing.expect(program.statements.items.len == literals.len) catch |err| {
        std.debug.print("{d} != {d}\n", .{ program.statements.items.len, literals.len });
        return err;
    };

    for (literals, 0..) |literal, i| {
        const stmt = program.statements.items[i];
        testing.expect(std.mem.eql(u8, stmt.literal(), literal)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ stmt.literal(), literal });
            return err;
        };
    }
}

test "identifier expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var p = Parser.init(Lexer.init("foobar;"));
    const program = p.parseProgram(allocator) catch |err| {
        std.debug.print("parseProgram failed with = {any}\n", .{err});
        return err;
    };

    testing.expect(program.statements.items.len == 1) catch |err| {
        std.debug.print("{d} != 1\n", .{program.statements.items.len});
        return err;
    };

    const stmt: ast.Statement = program.statements.getLast();

    return switch (stmt) {
        .expression => {
            try switch (stmt.expression.expression) {
                .identifier => testing.expect(std.mem.eql(u8, "foobar", stmt.expression.literal())),
                else => ParseError.InternalError,
            };
        },
        else => ParseError.InternalError,
    };
}

test "integer literal expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var p = Parser.init(Lexer.init("5;"));
    const program = p.parseProgram(allocator) catch |err| {
        std.debug.print("parseProgram failed with = {any}\n", .{err});
        return err;
    };

    testing.expect(program.statements.items.len == 1) catch |err| {
        std.debug.print("{d} != 1\n", .{program.statements.items.len});
        return err;
    };

    const stmt: ast.Statement = program.statements.getLast();

    return switch (stmt) {
        .expression => {
            try switch (stmt.expression.expression) {
                .integer_literal => testing.expect(std.mem.eql(u8, "5", stmt.expression.literal())),
                else => ParseError.InternalError,
            };
        },
        else => ParseError.InternalError,
    };
}

test "prefix expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const tests = [_]struct {
        program: []const u8,
        operator: []const u8,
        expectedLiteral: []const u8,
    }{
        .{ .program = "!5;", .operator = "!", .expectedLiteral = "5" },
        .{ .program = "-15;", .operator = "-", .expectedLiteral = "15" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };

        testing.expect(program.statements.items.len == 1) catch |err| {
            std.debug.print("{d} != 1\n", .{program.statements.items.len});
            return err;
        };

        const stmt: ast.Statement = program.statements.getLast();

        testing.expect(std.mem.eql(u8, case.expectedLiteral, stmt.expression.expression.prefix.right.literal())) catch |err| {
            std.debug.print("{s} != {s}\n", .{ case.expectedLiteral, stmt.expression.expression.prefix.right.literal() });
            return err;
        };

        testing.expect(std.mem.eql(u8, case.operator, stmt.expression.expression.prefix.operator)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ case.operator, stmt.expression.expression.prefix.operator });
            return err;
        };
    }
}

test "infix expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const tests = [_]struct {
        program: []const u8,
        operator: []const u8,
        expectedRightLiteral: []const u8,
        expectedLeftLiteral: []const u8,
    }{
        .{ .program = "5+5;", .operator = "+", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5*5;", .operator = "*", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5/5;", .operator = "/", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5>5;", .operator = ">", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5<5;", .operator = "<", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5==5;", .operator = "==", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
        .{ .program = "5!=5;", .operator = "!=", .expectedRightLiteral = "5", .expectedLeftLiteral = "5" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };

        testing.expect(program.statements.items.len == 1) catch |err| {
            std.debug.print("{d} != 1\n", .{program.statements.items.len});
            return err;
        };

        const stmt: ast.Statement = program.statements.getLast();

        testing.expect(std.mem.eql(u8, case.expectedRightLiteral, stmt.expression.expression.infix.right.literal())) catch |err| {
            std.debug.print("{s} != {s}\n", .{ case.expectedRightLiteral, stmt.expression.expression.infix.right.literal() });
            return err;
        };

        testing.expect(std.mem.eql(u8, case.expectedLeftLiteral, stmt.expression.expression.infix.left.literal())) catch |err| {
            std.debug.print("{s} != {s}\n", .{ case.expectedLeftLiteral, stmt.expression.expression.infix.left.literal() });
            return err;
        };

        testing.expect(std.mem.eql(u8, case.operator, stmt.expression.expression.infix.operator)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ case.operator, stmt.expression.expression.infix.operator });
            return err;
        };
    }
}

// NOTE: i have not implemented converting programs to strings, maninly because
// it felt a bit like a hassle. The main problem was related with string allocations
// and proper management of memory in the context of it. I need to think a bit deeper
// how that would work so that (in a recursion fashion too) with limited memory
// that needs to be properly freed. I could implement this without caring for memory
// allocation?
test "complex infix expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var p = Parser.init(Lexer.init("a+b/c;"));

    const program = p.parseProgram(allocator) catch |err| {
        std.debug.print("parseProgram failed with = {any}\n", .{err});
        return err;
    };
    defer program.deinit();

    testing.expect(program.statements.items.len == 1) catch |err| {
        std.debug.print("{d} != 1\n", .{program.statements.items.len});
        return err;
    };

    const stmt: ast.Statement = program.statements.getLast();

    const infix = stmt.expression.expression.infix;

    testing.expect(std.mem.eql(u8, infix.left.literal(), "a")) catch |err| {
        std.debug.print("{s} != {s}\n", .{ infix.left.literal(), "a" });
        return err;
    };

    const right = infix.right.infix;
    testing.expect(std.mem.eql(u8, right.operator, "/")) catch |err| {
        std.debug.print("{s} != {s}\n", .{ right.operator, "/" });
        return err;
    };
    testing.expect(std.mem.eql(u8, right.left.literal(), "b")) catch |err| {
        std.debug.print("{s} != {s}\n", .{ right.left.literal(), "b" });
        return err;
    };
    testing.expect(std.mem.eql(u8, right.right.literal(), "c")) catch |err| {
        std.debug.print("{s} != {s}\n", .{ right.left.literal(), "c" });
        return err;
    };
}

test "Parser.parseLetStatement" {
    const tests = [_]struct {
        program: []const u8,
        expectedLetIdent: ast.Identifier,
    }{
        .{ .program = "let five = 5;", .expectedLetIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.ident, .literal = "five" } } },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const let = try p.parseLetStatement();

        testing.expect(let.let.name.token.tokenType == case.expectedLetIdent.token.tokenType) catch |err| {
            std.debug.print("{any} != {any}\n", .{ let.let.name.token.tokenType, case.expectedLetIdent.token.tokenType });
            return err;
        };
        testing.expect(std.mem.eql(u8, let.let.name.token.literal, case.expectedLetIdent.token.literal)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ let.let.name.token.literal, case.expectedLetIdent.token.literal });
            return err;
        };
    }
}

test "Parser.parseLetStatement fail" {
    const tests = [_]struct {
        program: []const u8,
        expectedErr: ParseError,
    }{
        .{ .program = "let @ = 5;", .expectedErr = ParseError.MissingIdentOrLiteral },
        .{ .program = "let five;", .expectedErr = ParseError.MissingAssign },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const let = p.parseLetStatement();
        try testing.expectError(case.expectedErr, let);
    }
}

test "Parser.parseReturnStatement" {
    const tests = [_]struct {
        program: []const u8,
        expectedReturnIdent: ast.Identifier,
    }{
        .{ .program = "return five;", .expectedReturnIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.ident, .literal = "five" } } },
        .{ .program = "return 5;", .expectedReturnIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.int, .literal = "5" } } },
        .{ .program = "return true;", .expectedReturnIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.boolTrue, .literal = tokenType.boolTrue.string() } } },
        .{ .program = "return false;", .expectedReturnIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.boolFalse, .literal = tokenType.boolFalse.string() } } },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const rw = try p.parseReturnStatement();

        testing.expect(rw.return_with.identifier.token.tokenType == case.expectedReturnIdent.token.tokenType) catch |err| {
            std.debug.print("{any} != {any}\n", .{ rw.return_with.identifier.token.tokenType, case.expectedReturnIdent.token.tokenType });
            return err;
        };
        testing.expect(std.mem.eql(u8, rw.return_with.identifier.token.literal, case.expectedReturnIdent.token.literal)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ rw.return_with.identifier.token.literal, case.expectedReturnIdent.token.literal });
            return err;
        };
    }
}

test "Parser.parseReturnStatement fail" {
    const tests = [_]struct {
        program: []const u8,
        expectedErr: ParseError,
    }{
        .{ .program = "return @;", .expectedErr = ParseError.MissingIdentOrLiteral },
        .{ .program = "return ;", .expectedErr = ParseError.MissingIdentOrLiteral },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const rw = p.parseReturnStatement();
        try testing.expectError(case.expectedErr, rw);
    }
}

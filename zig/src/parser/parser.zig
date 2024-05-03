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

    // If errors
    MissingOpeningBrace,
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
                .let => try self.parseLetStatement(&prog),
                .returnWith => try self.parseReturnStatement(&prog),
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

    fn parseReturnStatement(self: *Parser, program: *ast.Program) ParseError!ast.Statement {
        // move to the next token in order to parse the actual values of the expression
        // being returned
        const cur = self.current_token;
        self.nextToken();

        const exp = try self.parseExpression(@intFromEnum(Operator.lowest), program);

        const returnStmt = ast.ReturnStatement{
            .token = cur,
            .expression = exp,
        };

        // skip until we find a semi colon so that the parsing is ready for
        // the next statement
        while (self.current_token.tokenType != tokenType.semicolon) {
            self.nextToken();
        }

        return ast.Statement{ .return_with = returnStmt };
    }

    fn parseLetStatement(self: *Parser, prog: *ast.Program) ParseError!ast.Statement {
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

        // move over the token after the assign so that we can call parseExpression
        self.nextToken();
        self.nextToken();

        let.value = try self.parseExpression(Operator.precedence(self.current_token.tokenType), prog);

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
        while (self.current_token.tokenType != tokenType.semicolon and self.current_token.tokenType != tokenType.eof) {
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
            .boolTrue, .boolFalse => Parser.parseBooleanLiteral,
            .bang, .minus => Parser.parsePrefixExpression,
            .lparen => Parser.parseGroupedExpression,
            .ifCond => Parser.parseIfExpression,
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
        var left_exp = try parseFn(self, program);

        // we exit when we find a semicolon or the precedence of the operator is
        // greater than the precedence of the next token
        while ((self.peek_token.tokenType != tokenType.semicolon and precedence < Operator.precedence(self.peek_token.tokenType)) or self.peek_token.tokenType == tokenType.eof) {
            // if we don't find any we need to return the left expression
            const infixFn = infixParseFn(self.peek_token.tokenType) catch {
                // break of the loop to return leftExp below
                break;
            };

            // move on to the next token, parse the new expression and re-assign
            // it. This is so that more complicated expressions that have many
            // infix operations within can be created properly
            self.nextToken();

            left_exp = try infixFn(self, program, left_exp);
        }

        return left_exp;
    }

    // parseInfixExpression gets called once the `left` expression has already
    // been parsed
    fn parseInfixExpression(self: *Parser, program: *ast.Program, left: ast.Expression) ParseError!ast.Expression {
        const alloc = program.arena.allocator();

        const cur_token = self.current_token;
        const prec = Operator.precedence(cur_token.tokenType);

        self.nextToken();
        const right = try self.parseExpression(prec, program);

        // allocate a pointer for the right expression
        // progra

        const right_ptr = alloc.create(ast.Expression) catch |err| {
            std.debug.print("could not allocate expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        program.expressions.append(right_ptr) catch |err| {
            std.debug.print("could not append expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        right_ptr.* = right;

        // allocate a pointer for the left expression
        const left_ptr = alloc.create(ast.Expression) catch |err| {
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

    // NOTE: I'm not entirely sure if I'm handling allocations the right way.
    // But I want to finish the book and maybe later learn a bit better
    // how we could structure things in a more native zig way. Right now it feels patched
    // to make it work with how the Go version of the interpreter is written
    fn parsePrefixExpression(self: *Parser, program: *ast.Program) ParseError!ast.Expression {
        const operator_token = self.current_token;

        // if we are in here then we've already identified that we are dealing
        // with a prefix expression (e.g. '!5'). So we need to move over to
        // get the next token and obtain the prefixParseFn of that one
        self.nextToken();

        const right = try self.parseExpression(@intFromEnum(Operator.prefix), program);

        const right_ptr = program.arena.allocator().create(ast.Expression) catch |err| {
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

    fn parseGroupedExpression(self: *Parser, program: *ast.Program) ParseError!ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(@intFromEnum(Operator.lowest), program);
        if (self.peek_token.tokenType != tokenType.rparen) {
            return ParseError.InternalError;
        }

        // ignore the rparen and leave the parser ready to parse the next stmt
        self.nextToken();

        return exp;
    }

    fn parseIfExpression(self: *Parser, program: *ast.Program) ParseError!ast.Expression {
        const alloc = program.arena.allocator();

        // when we get inside the parser has:
        // - current_token = if
        // - peek_token = lparen
        // so we need to move over to lparen to parse the expression properly
        const if_token = self.current_token;
        self.nextToken();

        const cond_exp = try self.parseExpression(@intFromEnum(Operator.lowest), program);

        const cond_ptr = alloc.create(ast.Expression) catch |err| {
            std.debug.print("could not allocate expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        program.expressions.append(cond_ptr) catch |err| {
            std.debug.print("could not append expression: {any}\n", .{err});
            return ParseError.InternalError;
        };

        cond_ptr.* = cond_exp;

        if (self.peek_token.tokenType != tokenType.lbrace) {
            return ParseError.MissingOpeningBrace;
        }

        // move on so that current token is lbrace
        self.nextToken();

        var cond_stmt = ast.BlockStatement{
            .token = if_token,
            .statements = std.ArrayList(ast.Statement).init(alloc),
        };
        errdefer cond_stmt.statements.deinit();

        if (self.peek_token.tokenType != tokenType.rbrace) {
            self.nextToken();

            while (self.current_token.tokenType != tokenType.rbrace) {
                const stmt = switch (self.current_token.tokenType) {
                    .let => try self.parseLetStatement(program),
                    .returnWith => try self.parseReturnStatement(program),
                    else => try self.parseExpressionStatement(program),
                };

                // skip semicolon so that parser is ready for next stmt or expression
                self.nextToken();

                // we only reach here once an actual statement has been initialized
                cond_stmt.statements.append(stmt) catch |err| {
                    std.debug.print("could not append statements: {any}\n", .{err});
                    return ParseError.InternalError;
                };
            }
        }

        const empty_block = ast.BlockStatement{
            .token = if_token,
            .statements = std.ArrayList(ast.Statement).init(alloc),
        };
        errdefer empty_block.statements.deinit();

        return ast.Expression{
            .ifExpression = ast.IfExpression{
                .token = if_token,
                .condition = cond_ptr,
                .consequence = cond_stmt,
                .alternative = empty_block,
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

    fn parseBooleanLiteral(self: *Parser, _: *ast.Program) ParseError!ast.Expression {
        return ast.Expression{
            .boolean_literal = ast.BooleanLiteral{
                .token = self.current_token,
            },
        };
    }
};

test "Parser.parseProgram" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.init(Lexer.init("5;"));
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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
        defer program.deinit();

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
    const allocator = std.testing.allocator;

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
        .{ .program = "true != false;", .operator = "!=", .expectedRightLiteral = "false", .expectedLeftLiteral = "true" },
        .{ .program = "true == false;", .operator = "==", .expectedRightLiteral = "false", .expectedLeftLiteral = "true" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        // no need to deinit, arena allocator takes care of it
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

test "embedded expressions" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedString: []const u8,
    }{
        .{ .program = "-a;", .expectedString = "(-a)\n" },
        .{ .program = "-a * b;", .expectedString = "((-a)*b)\n" },
        .{ .program = "!-a;", .expectedString = "(!(-a))\n" },
        .{ .program = "a+b+c;", .expectedString = "((a+b)+c)\n" },
        .{ .program = "a+b-c;", .expectedString = "((a+b)-c)\n" },
        .{ .program = "a*b*c;", .expectedString = "((a*b)*c)\n" },
        .{ .program = "a*b/c;", .expectedString = "((a*b)/c)\n" },
        .{ .program = "a+b/c;", .expectedString = "(a+(b/c))\n" },
        .{ .program = "a+b*c+d/e-f;", .expectedString = "(((a+(b*c))+(d/e))-f)\n" },
        .{ .program = "3+4; -5 * 5;", .expectedString = "(3+4)\n((-5)*5)\n" },
        .{ .program = "5>4==3<4;", .expectedString = "((5>4)==(3<4))\n" },
        .{ .program = "5>4!=3<4;", .expectedString = "((5>4)!=(3<4))\n" },
        .{ .program = "3 + 4*5 == 3*1 + 4*5;", .expectedString = "((3+(4*5))==((3*1)+(4*5)))\n" },
        .{ .program = "true==false;", .expectedString = "(true==false)\n" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };
        defer program.deinit();

        const value = try program.string(allocator);
        defer allocator.free(value);

        testing.expect(std.mem.eql(u8, value, case.expectedString)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ value, case.expectedString });
            return err;
        };
    }
}

test "grouped expressions" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedString: []const u8,
    }{
        .{ .program = "1 + (2 + 3) + 4;", .expectedString = "((1+(2+3))+4)\n" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };
        defer program.deinit();

        const value = try program.string(allocator);
        defer allocator.free(value);

        testing.expect(std.mem.eql(u8, value, case.expectedString)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ value, case.expectedString });
            return err;
        };
    }
}

test "if expressions" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedString: []const u8,
    }{
        .{ .program = "if (x > y) { return x; }", .expectedString = "if ((x>y)) {\nreturn x\n} else {}\n" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };
        defer program.deinit();

        const value = try program.string(allocator);
        defer allocator.free(value);

        testing.expect(std.mem.eql(u8, value, case.expectedString)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ value, case.expectedString });
            return err;
        };
    }
}

test "program printing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        program: []const u8,
        expectedString: []const u8,
    }{
        .{ .program = "let five = 5;", .expectedString = "let five = 5\n" },
        .{ .program = "let five = add(x,y);", .expectedString = "let five = add\n" },
        .{ .program = "return five;", .expectedString = "return five\n" },
        .{ .program = "return 5;", .expectedString = "return 5\n" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(allocator) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };
        defer program.deinit();

        const value = try program.string(allocator);
        testing.expect(std.mem.eql(u8, value, case.expectedString)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ value, case.expectedString });
            return err;
        };
    }
}

test "Parser.parseLetStatement" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedLetIdent: ast.Identifier,
    }{
        .{ .program = "let five = 5;", .expectedLetIdent = ast.Identifier{ .token = Token{ .tokenType = tokenType.ident, .literal = "five" } } },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        var prog = ast.Program.init(allocator);
        defer prog.deinit();

        const let = try p.parseLetStatement(&prog);

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
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedErr: ParseError,
    }{
        .{ .program = "let @ = 5;", .expectedErr = ParseError.MissingIdentOrLiteral },
        .{ .program = "let five;", .expectedErr = ParseError.MissingAssign },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        var prog = ast.Program.init(allocator);
        defer prog.deinit();
        const let = p.parseLetStatement(&prog);
        try testing.expectError(case.expectedErr, let);
    }
}

test "Parser.parseReturnStatement" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedReturn: []const u8,
    }{
        .{ .program = "return five;", .expectedReturn = "return five" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        var prog = ast.Program.init(allocator);
        defer prog.deinit();

        const rw = try p.parseReturnStatement(&prog);
        const result = try rw.string(allocator);
        defer allocator.free(result);

        testing.expect(std.mem.eql(u8, result, case.expectedReturn)) catch |err| {
            std.debug.print("{s} != {s}\n", .{ result, case.expectedReturn });
            return err;
        };
    }
}

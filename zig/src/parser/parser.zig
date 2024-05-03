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

    // Block statement errors
    MissingOpeningBrace,
    MissingClosingBrace,

    // fn errors
    MissingOpeningParen,
    MissingClosingParen,
    InvalidFunctionParameters,
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
            .function => Parser.parseFunctionLiteral,
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
        errdefer alloc.destroy(right_ptr);

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
        errdefer alloc.destroy(left_ptr);

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
        errdefer alloc.destroy(cond_ptr);

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

        // exiting parseBlockStatement means the current_token will be the closign
        // brace of this conditional block
        const cond_block = try self.parseBlockStatement(if_token, program);

        // if peek token is not `else` then we return the if expression with an
        // empty block for the alternative
        if (self.peek_token.tokenType != tokenType.elseCond) {
            var empty_block = ast.BlockStatement{
                .token = if_token,
                .statements = std.ArrayList(ast.Statement).init(alloc),
            };
            errdefer empty_block.statements.deinit();
            return ast.Expression{
                .if_expression = ast.IfExpression{
                    .token = if_token,
                    .condition = cond_ptr,
                    .consequence = cond_block,
                    .alternative = empty_block,
                },
            };
        }

        // get the else token to then parse the block of statements
        self.nextToken();
        const else_token = self.current_token;

        // token that comes after the else should be an opening brace given
        // that its a complete block statement, making sure this is the case
        // is taken care of by parseBlockStatement
        self.nextToken();
        const else_block = try self.parseBlockStatement(else_token, program);

        return ast.Expression{
            .if_expression = ast.IfExpression{
                .token = if_token,
                .condition = cond_ptr,
                .consequence = cond_block,
                .alternative = else_block,
            },
        };
    }

    fn parseFunctionLiteral(self: *Parser, program: *ast.Program) ParseError!ast.Expression {
        const alloc = program.arena.allocator();

        if (self.peek_token.tokenType != tokenType.lparen) {
            return ParseError.MissingOpeningParen;
        }

        const fn_token = self.current_token;
        self.nextToken();

        var fn_literal = ast.FunctionLiteral{
            .token = fn_token,
            .parameters = std.ArrayList(ast.Identifier).init(alloc),
            .block = ast.BlockStatement{
                .token = fn_token,
                .statements = std.ArrayList(ast.Statement).init(alloc),
            },
        };

        // we start with current token being lparen so we parse the list of parameters
        if (self.peek_token.tokenType != tokenType.rparen) {
            while (self.current_token.tokenType != tokenType.rparen) {
                if (self.current_token.tokenType == tokenType.eof) {
                    return ParseError.MissingClosingParen;
                }

                // move over so that current token can be interpreted as an identifier
                self.nextToken();
                // if the next token is not a comma and not an rparen then its invalid
                if (self.peek_token.tokenType != tokenType.comma and self.peek_token.tokenType != tokenType.rparen) {
                    return ParseError.InvalidFunctionParameters;
                }

                // parse the current identifier and append it to the list
                const ident = try parseIdentifier(self, program);
                fn_literal.parameters.append(ident.identifier) catch |err| {
                    std.debug.print("could not append identifier to parameter list: {any}\n", .{err});
                    return ParseError.InternalError;
                };

                // move over ignoring
                self.nextToken();
            }
        } else {
            self.nextToken();
        }

        // when we get here current token is ) so peek token should be opening brace
        if (self.peek_token.tokenType != tokenType.lbrace) {
            return ParseError.MissingOpeningBrace;
        }
        // we move next token to be opening brace so that we can then call
        // parseBlockStatement
        self.nextToken();

        fn_literal.block = try self.parseBlockStatement(fn_token, program);

        return ast.Expression{
            .function_literal = fn_literal,
        };
    }

    // parseBlockStatement assumes that the parser is ready to start parsing
    // a block of statements, this means that the current token must be the
    // opening brace. When this function exits the current token will be the
    // closing right brace (tokenType.rbrace)
    fn parseBlockStatement(self: *Parser, token: Token, program: *ast.Program) ParseError!ast.BlockStatement {
        const alloc = program.arena.allocator();

        if (self.current_token.tokenType != tokenType.lbrace) {
            return ParseError.MissingOpeningBrace;
        }

        var block = ast.BlockStatement{
            .token = token,
            .statements = std.ArrayList(ast.Statement).init(alloc),
        };
        errdefer block.statements.deinit();

        self.nextToken();

        if (self.current_token.tokenType == tokenType.rbrace) {
            return block;
        }

        while (self.current_token.tokenType != tokenType.rbrace) {
            if (self.current_token.tokenType == tokenType.eof) {
                return ParseError.MissingClosingBrace;
            }

            const stmt = switch (self.current_token.tokenType) {
                .let => try self.parseLetStatement(program),
                .returnWith => try self.parseReturnStatement(program),
                else => try self.parseExpressionStatement(program),
            };

            self.nextToken();

            block.statements.append(stmt) catch |err| {
                std.debug.print("could not append statements: {any}\n", .{err});
                return ParseError.InternalError;
            };
        }

        return block;
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
        .{ .program = "if (x > y) { return x; } else {}", .expectedString = "if ((x>y)) {\nreturn x\n} else {}\n" },
        .{ .program = "if (x > y) {} else { return (2+3); }", .expectedString = "if ((x>y)) {} else {\nreturn (2+3)\n}\n" },
        .{ .program = "if (x > y) {} else {}", .expectedString = "if ((x>y)) {} else {}\n" },
        .{ .program = "if (x > y) { let five = 5;\nreturn five; } else { let two = 2;\nreturn two; }", .expectedString = "if ((x>y)) {\nlet five = 5\nreturn five\n} else {\nlet two = 2\nreturn two\n}\n" },
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

test "function literals" {
    const alloc = std.testing.allocator;

    const tests = [_]struct {
        program: []const u8,
        expectedString: []const u8,
    }{
        .{ .program = "fn () {}", .expectedString = "fn () {}\n" },
        .{ .program = "fn (one, two, three) {}", .expectedString = "fn (one,two,three) {}\n" },
        .{ .program = "fn (one, two, three) {let five = 5;\nreturn five;}", .expectedString = "fn (one,two,three) {\nlet five = 5\nreturn five\n}\n" },
    };

    inline for (tests) |case| {
        var p = Parser.init(Lexer.init(case.program));

        const program = p.parseProgram(alloc) catch |err| {
            std.debug.print("parseProgram failed with = {any}\n", .{err});
            return err;
        };
        defer program.deinit();

        const value = try program.string(alloc);
        defer alloc.free(value);

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

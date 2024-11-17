const token = @import("token");
const std = @import("std");
const dprint = std.debug.print;
const testing = std.testing;

input: []const u8,
position: usize,
readPosition: usize,
ch: u8,

pub const Lexer = @This();

pub fn init(input: []const u8) Lexer {
    var l = Lexer{
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = 0,
    };
    l.readChar();
    return l;
}

fn readChar(self: *Lexer) void {
    if (self.readPosition >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.readPosition];
    }

    self.position = self.readPosition;
    self.readPosition += 1;
}

fn readValue(self: *Lexer, comptime check: fn (u8) bool) []const u8 {
    const position = self.position;
    while (check(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn skipWhitespace(self: *Lexer) void {
    while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
        self.readChar();
    }
}

fn peekChar(self: Lexer) u8 {
    if (self.readPosition >= self.input.len) {
        return 0;
    } else {
        return self.input[self.readPosition];
    }
}

pub fn nextToken(self: *Lexer) token.Token {
    var tok: token.Token = undefined;
    self.skipWhitespace();

    switch (self.ch) {
        token.TokenType.assign.string()[0] => {
            if (self.peekChar() == token.TokenType.assign.string()[0]) {
                self.readChar();
                tok.tokenType = token.TokenType.eq;
                tok.literal = token.TokenType.eq.string();
            } else {
                tok.tokenType = token.TokenType.assign;
                tok.literal = "=";
            }
        },
        token.TokenType.semicolon.string()[0] => {
            tok.tokenType = token.TokenType.semicolon;
            tok.literal = ";";
        },
        token.TokenType.lparen.string()[0] => {
            tok.tokenType = token.TokenType.lparen;
            tok.literal = "(";
        },
        token.TokenType.rparen.string()[0] => {
            tok.tokenType = token.TokenType.rparen;
            tok.literal = ")";
        },
        token.TokenType.comma.string()[0] => {
            tok.tokenType = token.TokenType.comma;
            tok.literal = ",";
        },
        token.TokenType.plus.string()[0] => {
            tok.tokenType = token.TokenType.plus;
            tok.literal = "+";
        },
        token.TokenType.minus.string()[0] => {
            tok.tokenType = token.TokenType.minus;
            tok.literal = "-";
        },
        token.TokenType.bang.string()[0] => {
            if (self.peekChar() == token.TokenType.assign.string()[0]) {
                self.readChar();
                tok.tokenType = token.TokenType.notEq;
                tok.literal = token.TokenType.notEq.string();
            } else {
                tok.tokenType = token.TokenType.bang;
                tok.literal = "!";
            }
        },
        token.TokenType.slash.string()[0] => {
            tok.tokenType = token.TokenType.slash;
            tok.literal = "/";
        },
        token.TokenType.asterisk.string()[0] => {
            tok.tokenType = token.TokenType.asterisk;
            tok.literal = "*";
        },
        token.TokenType.lt.string()[0] => {
            tok.tokenType = token.TokenType.lt;
            tok.literal = "<";
        },
        token.TokenType.gt.string()[0] => {
            tok.tokenType = token.TokenType.gt;
            tok.literal = ">";
        },
        token.TokenType.lbrace.string()[0] => {
            tok.tokenType = token.TokenType.lbrace;
            tok.literal = "{";
        },
        token.TokenType.rbrace.string()[0] => {
            tok.tokenType = token.TokenType.rbrace;
            tok.literal = "}";
        },
        '0'...'9' => {
            tok.tokenType = token.TokenType.int;
            tok.literal = self.readValue(isDigit);
            return tok;
        },
        'a'...'z', 'A'...'Z', '_' => {
            tok.literal = self.readValue(isLetter);
            tok.tokenType = token.TokenType.LookupIdent(tok.literal);
            return tok;
        },
        0 => {
            tok.tokenType = token.TokenType.eof;
            tok.literal = "";
        },
        else => {
            tok.tokenType = token.TokenType.illegal;
            tok.literal = "";
        },
    }

    self.readChar();

    return tok;
}

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "Lexer.nextToken" {
    const tests = [_]struct {
        expectedType: token.TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = token.TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "five" },
        .{ .expectedType = token.TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "ten" },
        .{ .expectedType = token.TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "add" },
        .{ .expectedType = token.TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenType.function, .expectedLiteral = "fn" },
        .{ .expectedType = token.TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "x" },
        .{ .expectedType = token.TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "y" },
        .{ .expectedType = token.TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "x" },
        .{ .expectedType = token.TokenType.plus, .expectedLiteral = "+" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "y" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "result" },
        .{ .expectedType = token.TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "add" },
        .{ .expectedType = token.TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "five" },
        .{ .expectedType = token.TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = token.TokenType.ident, .expectedLiteral = "ten" },
        .{ .expectedType = token.TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.bang, .expectedLiteral = "!" },
        .{ .expectedType = token.TokenType.minus, .expectedLiteral = "-" },
        .{ .expectedType = token.TokenType.slash, .expectedLiteral = "/" },
        .{ .expectedType = token.TokenType.asterisk, .expectedLiteral = "*" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenType.lt, .expectedLiteral = "<" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.gt, .expectedLiteral = ">" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.ifCond, .expectedLiteral = "if" },
        .{ .expectedType = token.TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenType.lt, .expectedLiteral = "<" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenType.returnWith, .expectedLiteral = "return" },
        .{ .expectedType = token.TokenType.boolTrue, .expectedLiteral = "true" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenType.elseCond, .expectedLiteral = "else" },
        .{ .expectedType = token.TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenType.returnWith, .expectedLiteral = "return" },
        .{ .expectedType = token.TokenType.boolFalse, .expectedLiteral = "false" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.eq, .expectedLiteral = "==" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenType.notEq, .expectedLiteral = "!=" },
        .{ .expectedType = token.TokenType.int, .expectedLiteral = "9" },
        .{ .expectedType = token.TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.eof, .expectedLiteral = "" },
    };

    const input =
        \\let five = 5;
        \\
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
        \\
        \\!-/*5;
        \\
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\10 == 10;
        \\10 != 9;
    ;
    var l = Lexer.init(input);

    inline for (tests) |case| {
        const tok = l.nextToken();

        testing.expect(tok.tokenType == case.expectedType) catch |err| {
            dprint("{any} != {any}\n", .{ tok.tokenType, case.expectedType });
            return err;
        };
        testing.expect(std.mem.eql(u8, tok.literal, case.expectedLiteral)) catch |err| {
            dprint("{s} != {s}\n", .{ tok.literal, case.expectedLiteral });
            return err;
        };
    }
}

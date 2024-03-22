const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    ident,
    int,

    // operatos
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    notEq,

    // delimiters
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,

    // keywords
    function,
    let,
    boolTrue,
    boolFalse,
    ifCond,
    elseCond,
    returnWith,

    const keywords = std.ComptimeStringMap(TokenType, .{
        .{ TokenType.function.string(), TokenType.function },
        .{ TokenType.let.string(), TokenType.let },
        .{ TokenType.boolTrue.string(), TokenType.boolTrue },
        .{ TokenType.boolFalse.string(), TokenType.boolFalse },
        .{ TokenType.ifCond.string(), TokenType.ifCond },
        .{ TokenType.elseCond.string(), TokenType.elseCond },
        .{ TokenType.returnWith.string(), TokenType.returnWith },
    });

    pub fn string(tokenType: TokenType) []const u8 {
        switch (tokenType) {
            .eof => return "",
            .assign => return "=",
            .plus => return "+",
            .minus => return "-",
            .bang => return "!",
            .asterisk => return "*",
            .slash => return "/",
            .lt => return "<",
            .gt => return ">",
            .eq => return "==",
            .notEq => return "!=",
            .comma => return ",",
            .semicolon => return ";",
            .lparen => return "(",
            .rparen => return ")",
            .lbrace => return "{",
            .rbrace => return "}",
            .function => return "fn",
            .let => return "let",
            .boolTrue => return "true",
            .boolFalse => return "false",
            .ifCond => return "if",
            .elseCond => return "else",
            .returnWith => return "return",
            else => return "unknown",
        }
    }

    pub fn LookupIdent(ident: []const u8) TokenType {
        return keywords.get(ident) orelse TokenType.ident;
    }
};

pub const Token = struct {
    tokenType: TokenType,
    literal: []const u8,
};

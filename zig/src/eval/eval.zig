const std = @import("std");
const Parser = @import("../parser/parser.zig").Parser;
const ast = @import("../ast/ast.zig");

// top level EvalStatement called initially that looks for each of let, return_with and expression
//
// EvalExpression receives an Expression and evals it
//
// program starts by iterating over statements and calling EvalStatement
// EvalStatement will then evaluate the subtype that corresponds and call separate
// functions for each subtype

// EvalStatement evaluates top level statements which derive in three
// different kinds: let, return and expressions
pub fn EvalStatement(stmt: ast.Statement) void {
    switch (stmt) {
        .let, .return_with => return,
        .expression => EvalExpression(stmt.expression.expression),
    }
}

pub fn EvalExpression(exp: ast.Expression) void {
    switch (exp) {
        .integer_literal => 
    }
    return;
}

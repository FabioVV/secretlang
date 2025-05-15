const std = @import("std");
const io = std.io;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const AST = @import("ast.zig");

pub const DEBUG_PRINT_TOKENS: bool = false;
pub const DEBUG_PRINT_VAR_STATEMENT: bool = false;
pub const DEBUG_PRINT_PREFIX_EXPRESSION: bool = true;

pub fn printToken(token: Token) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_TOKENS) return;

    stdoutwriter.print("Token: {s} - Literal: {s}\n", .{ @tagName(token.token_type), token.literal }) catch |err| {
        std.debug.print("Error debug printing token: {any}", .{err});
    };
}

pub fn printVarStatement(stmt: AST.VarStatement) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_VAR_STATEMENT) return;

    const val = stmt.expression.*.number_expr.value;
    stdoutwriter.print("Token: {s} - Indentifier: {s} -> {d}\n", .{ @tagName(stmt.token.token_type), stmt.identifier.literal, val }) catch |err| {
        std.debug.print("Error debug printing var statement: {any}", .{err});
    };
}

pub fn printPrefixExpression(stmt: AST.PrefixExpression) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_PREFIX_EXPRESSION) return;

    const val = stmt.right.*.number_expr.value;
    stdoutwriter.print("Token: {s} - PrefixOperator: {s} -> {s}{d}\n", .{ @tagName(stmt.token.token_type), stmt.token.literal, stmt.token.literal, val }) catch |err| {
        std.debug.print("Error debug prefix expression: {any}", .{err});
    };
}

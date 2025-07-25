const std = @import("std");
const io = std.io;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;

const AST = @import("ast.zig");

pub const DEBUG_PRINT_TOKENS: bool = false;
pub const DEBUG_PRINT_VAR_STATEMENT: bool = false;
pub const DEBUG_PRINT_PREFIX_EXPRESSION: bool = false;
pub const DEBUG_PRINT_SIMPLE_IF_EXPRESSION: bool = false;
pub const DEBUG_PRINT_FN: bool = false;
pub const DEBUG_PRINT_FN_CALL: bool = false;

pub const ANSI_RESET = "\x1b[0m";
pub const ANSI_RED = "\x1b[31m";
pub const ANSI_GREEN = "\x1b[32m";
pub const ANSI_YELLOW = "\x1b[33m";
pub const ANSI_BLUE = "\x1b[34m";
pub const ANSI_MAGENTA = "\x1b[35m";
pub const ANSI_CYAN = "\x1b[36m";
pub const ANSI_WHITE = "\x1b[37m";

// UTILITIES ->
pub fn printNodes(stmt: AST.Statement) void {
    const stdoutwriter = io.getStdOut().writer();

    switch (stmt) {
        .var_stmt => |varStmt| {
            stdoutwriter.print("{s}Main AST node{s}: {any}\n", .{ ANSI_GREEN, ANSI_RESET, varStmt }) catch |err| {
                std.debug.print("Error printing var node: {any}", .{err});
            };

            stdoutwriter.print("{s}Var identifier{s}: {s}\n", .{ ANSI_GREEN, ANSI_RESET, varStmt.identifier.literal }) catch |err| {
                std.debug.print("Error printing var node: {any}", .{err});
            };

            printExpression(varStmt.expression.?, "Var identifier value");
        },
        .return_stmt => |returnStmt| {
            stdoutwriter.print("{s}Main AST node{s}: {any}\n", .{ ANSI_GREEN, ANSI_RESET, returnStmt }) catch |err| {
                std.debug.print("Error printing return node: {any}", .{err});
            };

            printExpression(returnStmt.expression, "Return value");
        },
        .expression_stmt => |exprStmt| {
            stdoutwriter.print("{s}Main AST node{s}: {any}\n", .{ ANSI_GREEN, ANSI_RESET, exprStmt }) catch |err| {
                std.debug.print("Error printing expression statement node: {any}", .{err});
            };

            printExpression(exprStmt.expression, "Expression statement");
        },
        else => {},
    }
}

pub fn printExpression(expr: ?*AST.Expression, message: []const u8) void {
    switch (expr.?.*) {
        AST.Expression.boolean_expr => |boolExpr| {
            std.debug.print("{s}{s}{s}: {}\n", .{ ANSI_GREEN, message, ANSI_RESET, boolExpr.value });
        },
        AST.Expression.number_expr => |numExpr| {
            std.debug.print("{s}{s}{s}: {d:6.5}\n", .{ ANSI_GREEN, message, ANSI_RESET, numExpr.value });
        },
        AST.Expression.string_expr => |strExpr| {
            std.debug.print("{s}{s}{s}: {s}\n", .{ ANSI_GREEN, message, ANSI_RESET, strExpr.value });
        },
        AST.Expression.identifier_expr => |idExpr| {
            std.debug.print("{s}{s}{s}: {s}\n", .{ ANSI_GREEN, message, ANSI_RESET, idExpr.literal });
        },
        AST.Expression.infix_expr => |infixExpr| {
            printExpression(infixExpr.left, "Infix left value");
            std.debug.print("{s}Operation{s}: {s}\n", .{ ANSI_GREEN, ANSI_RESET, infixExpr.token.literal });
            printExpression(infixExpr.right, "Infix right");
        },
        AST.Expression.prefix_expr => |prefixExpr| {
            std.debug.print("{s}{s}{s}: {s}\n", .{ ANSI_GREEN, message, ANSI_RESET, prefixExpr.token.literal });
            printExpression(prefixExpr.right, "Prefix right value");
        },
        AST.Expression.fn_expr => |fnExpr| {
            std.debug.print("{s}{s}{s}: {s}\n", .{ ANSI_GREEN, message, ANSI_RESET, fnExpr.token.literal });
        },
        AST.Expression.call_expr => |callExpr| {
            std.debug.print("{s}{s} of function call {s}:\n", .{ ANSI_GREEN, message, ANSI_RESET });
            printExpression(callExpr.function.?, "Functio call");
        },
        else => {},
    }
}

pub fn printTokenDebug(token: Token) void {
    const stdoutwriter = io.getStdOut().writer();

    stdoutwriter.print("Token: {s} - Literal: {s}\n", .{ @tagName(token.token_type), token.literal }) catch |err| {
        std.debug.print("Error debug printing token: {any}", .{err});
    };
}

// <- UTILITIES

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

pub fn printIfExpression(stmt: AST.IfExpression) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_SIMPLE_IF_EXPRESSION) return;

    const this_condition_needs_to_be_a_boolean_dont_change = stmt.condition.?.*.boolean_expr.value;

    stdoutwriter.print("Token: {s} - Condition: {} ->\n", .{ @tagName(stmt.token.token_type), this_condition_needs_to_be_a_boolean_dont_change }) catch |err| {
        std.debug.print("Error debug prefix expression: {any}", .{err});
    };

    for (stmt.ifBlock.?.statements.items) |node| {
        printNodes(node);
    }

    if (stmt.elseBlock != null) {
        stdoutwriter.print("ELSE BLOCK ->\n", .{}) catch |err| {
            std.debug.print("Error debug prefix expression: {any}", .{err});
        };

        for (stmt.elseBlock.?.statements.items) |node| {
            printNodes(node);
        }
    }
}

pub fn printFnExpression(stmt: AST.fnExpression) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_FN) return;

    stdoutwriter.print("Token: {s} - ->\n", .{@tagName(stmt.token.token_type)}) catch |err| {
        std.debug.print("Error debug print fn expression: {any}", .{err});
    };

    for (stmt.parameters.constSlice()) |param| {
        stdoutwriter.print("Param: {s}\n", .{param.literal}) catch |err| {
            std.debug.print("Error debug print fn expression params: {any}", .{err});
        };
    }

    if (stmt.body != null) {
        stdoutwriter.print("FUNCTION BODY ->\n", .{}) catch |err| {
            std.debug.print("Error debug print fn expression: {any}", .{err});
        };

        for (stmt.body.?.statements.items) |node| {
            printNodes(node);
        }
    }
}

pub fn printFnExpressionCall(stmt: AST.callExpression) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG_PRINT_FN_CALL) return;

    stdoutwriter.print("Token: {s} - ->\n", .{@tagName(stmt.token.token_type)}) catch |err| {
        std.debug.print("Error debug print fn expression: {any}", .{err});
    };

    printExpression(stmt.function, "Function");

    for (stmt.arguments.constSlice()) |param| {
        printExpression(param, "Arg");
    }
}

pub fn getSourceLine(source: []const u8, token: Token) []const u8 {
    const lineError = token.position.line;
    //const columnError = token.position.column;
    var currentLine: u32 = 0;
    var startLine: u32 = 0;

    var i: u32 = 0;
    while (i < source.len and currentLine < lineError) {
        if (source[i] == '\n') {
            currentLine += 1;
            startLine = i + 1;
        }
        i += 1;
    }

    var endLine = startLine;
    while (endLine < source.len and source[endLine] != '\n') {
        endLine += 1;
    }

    return source[startLine..endLine];
}

pub fn getSourceLineFromPosition(source: []const u8, pos: Position) []const u8 {
    const lineError = pos.line;
    //const columnError = pos.column;
    var currentLine: u32 = 0;
    var startLine: u32 = 0;

    var i: u32 = 0;
    while (i < source.len and currentLine < lineError) {
        if (source[i] == '\n') {
            currentLine += 1;
            startLine = i + 1;
        }
        i += 1;
    }

    var endLine = startLine;
    while (endLine < source.len and source[endLine] != '\n') {
        endLine += 1;
    }

    return source[startLine..endLine];
}

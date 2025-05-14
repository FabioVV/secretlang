const std = @import("std");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

pub const Program = struct {
    nodes: std.ArrayList(Statement),

    pub fn init() !Program {
        const program = Program{
            .nodes = std.ArrayList(Statement).init(std.heap.page_allocator),
        };
        return program;
    }

    pub fn addNode(self: *Program, stmt: Statement) !void {
        try self.nodes.append(stmt);
    }

    pub fn deinit(self: *Program) void {
        self.nodes.deinit();
    }
};

pub const NumberExpression = struct {
    token: Token,
    value: f64,
};

pub const StringExpression = struct {
    token: Token,
    value: []const u8,
};

pub const Statement = union(enum) {
    var_stmt: VarStatement,
    r_stmt: ReturnStatement,
    e_stmt: ExpressionStatement,
};

pub const VarStatement = struct {
    token: Token, // VAR token.
    identifier: Identifier,
    expression: *Expression, // the value being assigned to the var variable
};

pub const Identifier = struct {
    token: Token, // IDENT token.
    literal: []const u8,
};

pub const ReturnStatement = struct {
    token: Token, // RETURN token
    expression: *Expression = undefined, // the return value
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression = undefined,
};

pub const BinaryExpression = struct {
    token: Token,
    left: *Expression = undefined,
    right: *Expression = undefined,
};

pub const UnaryExpression = struct {
    token: Token,
    left: *Expression = undefined,
    right: *Expression = undefined,
};

pub const ExprTypes = enum {
    number_expr,
    string_expr,
    identifier_expr,
    binary_expr,
    unary_expr,
};

pub const Expression = union(ExprTypes) {
    number_expr: NumberExpression,
    string_expr: StringExpression,
    identifier_expr: Identifier,
    binary_expr: BinaryExpression,
    unary_expr: UnaryExpression,
};

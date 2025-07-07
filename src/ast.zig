const std = @import("std");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

pub const Program = struct {
    nodes: std.ArrayList(Statement),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) ?*Program {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const program = arena.allocator().create(Program) catch |err| {
            std.debug.print("Error allocating main AST for program: {any}", .{err});
            return null;
        };

        program.* = Program{
            .nodes = std.ArrayList(Statement).init(std.heap.page_allocator),
            .arena = arena,
        };
        return program;
    }

    pub fn addNode(self: *Program, stmt: Statement) !void {
        try self.nodes.append(stmt);
    }

    pub fn deinit(self: *Program) void {
        self.nodes.deinit();
        self.arena.deinit();
    }
};

pub const NumberExpression = struct { token: Token, value: f64 };

pub const StringExpression = struct { token: Token, value: []const u8 };

pub const BooleanExpression = struct { token: Token, value: bool };

pub const VarStatement = struct {
    token: Token, // VAR token.
    identifier: Identifier,
    expression: ?*Expression, // the value being assigned to the var variable
};

pub const Identifier = struct {
    token: Token, // IDENT token.
    literal: []const u8,
};

pub const ReturnStatement = struct {
    token: Token, // RETURN token
    expression: ?*Expression = undefined, // the return value
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: ?*Expression = undefined,
};

pub const InfixExpression = struct {
    token: Token,
    left: ?*Expression = undefined,
    right: ?*Expression = undefined,
};

pub const PrefixExpression = struct {
    token: Token,
    right: ?*Expression,
};

pub const StmtTypes = enum {
    var_stmt,
    r_stmt,
    e_stmt,
};

pub const Statement = union(StmtTypes) {
    var_stmt: VarStatement,
    r_stmt: ReturnStatement,
    e_stmt: ExpressionStatement,
};

pub const ExprTypes = enum {
    boolean_expr,
    number_expr,
    string_expr,
    identifier_expr,
    infix_expr,
    prefix_expr,
};

pub const Expression = union(ExprTypes) {
    boolean_expr: BooleanExpression,
    number_expr: NumberExpression,
    string_expr: StringExpression,
    identifier_expr: Identifier,
    infix_expr: InfixExpression,
    prefix_expr: PrefixExpression,
};

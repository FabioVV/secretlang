const std = @import("std");
const _token = @import("token.zig");
const Symbol = @import("symbol.zig").Symbol;
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
            .nodes = std.ArrayList(Statement).init(allocator),
            .arena = arena,
        };
        return program;
    }

    pub fn addNode(self: *Program, stmt: Statement) !void {
        try self.nodes.append(stmt);
    }

    pub fn deinit(self: *Program) void {
        for (self.nodes.items) |nodes| {
            switch (nodes) {
                .block_stmt => |block_stmt| {
                    block_stmt.statements.deinit();
                },
                .expression_stmt => {
                    // do nothing
                },
                else => {
                    // do nothing
                },
            }
        }
        self.nodes.deinit();
        self.arena.deinit();

        // Some of the frees in here my do nothing because since i used arena, technically, by calling self.arena.deinit(); it frees everything, but i will keep the individual frees anyway because it does not hurt. Also stuff li the block statements still need to be freed manually
    }
};

pub const NilExpression = struct { token: Token, value: void };

pub const NumberExpression = struct { token: Token, value: f64 };

pub const StringExpression = struct { token: Token, value: []const u8 };

pub const BooleanExpression = struct { token: Token, value: bool };

pub const VarStatement = struct {
    token: Token, // VAR token.
    identifier: Identifier,
    expression: ?*Expression, // the value being assigned to the var variable
};

pub const FnStatement = struct {
    token: Token, // fn token.
    identifier: Identifier, // name
    parameters: std.BoundedArray(Identifier, 32), // Max 32 params,
    body: ?*BlockStatement = null,

    pub fn init(token: Token) FnStatement {
        const fnStatement = FnStatement{
            .token = token,
            .parameters = .{},
            .identifier = undefined,
        };
        return fnStatement;
    }
};

pub const Identifier = struct {
    token: Token, // IDENT token.
    literal: []const u8,
    resolved_symbol: ?*Symbol = null,
};

pub const BlockStatement = struct {
    token: Token, // { token
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator, token: Token) BlockStatement {
        const block = BlockStatement{
            .token = token,
            .statements = std.ArrayList(Statement).init(allocator),
        };
        return block;
    }
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

pub const IfExpression = struct {
    token: Token,
    condition: ?*Expression = undefined,
    ifBlock: ?*BlockStatement = undefined,
    elseBlock: ?*BlockStatement = undefined,
};

pub const fnExpression = struct {
    token: Token,
    parameters: std.BoundedArray(Identifier, 32), // Max 32 params,
    body: ?*BlockStatement = undefined,

    pub fn init(token: Token) fnExpression {
        const fnLiteralExpression = fnExpression{
            .token = token,
            .parameters = .{},
        };
        return fnLiteralExpression;
    }
};

pub const callExpression = struct {
    token: Token, // ( token
    function: ?*Expression,
    arguments: std.BoundedArray(?*Expression, 32), // Max 32 arguments

    pub fn init(token: Token, fn_expr: ?*Expression) callExpression {
        const callExpr = callExpression{
            .token = token,
            .function = fn_expr,
            .arguments = .{},
        };
        return callExpr;
    }
};

pub const ArrayExpression = struct {
    token: Token, // [ token
    items: std.ArrayList(?*Expression),

    pub fn init(allocator: std.mem.Allocator, token: Token) ArrayExpression {
        const arr = ArrayExpression{
            .token = token,
            .items = std.ArrayList(?*Expression).init(allocator),
        };

        return arr;
    }
};

pub const IndexExpression = struct {
    token: Token, // [ token
    left: ?*Expression,
    index: ?*Expression,
};

pub const StmtTypes = enum {
    var_stmt,
    return_stmt,
    expression_stmt,
    block_stmt,
    fn_stmt,
};

pub const Statement = union(StmtTypes) {
    var_stmt: *VarStatement,
    return_stmt: *ReturnStatement,
    expression_stmt: *ExpressionStatement,
    block_stmt: *BlockStatement,
    fn_stmt: *FnStatement,
};

pub const ExprTypes = enum {
    nil_expr,
    boolean_expr,
    number_expr,
    string_expr,
    identifier_expr,
    infix_expr,
    prefix_expr,
    if_expr,
    fn_expr,
    call_expr,
    array_expr,
    index_expr,
    //hash_expr
};

pub const Expression = union(ExprTypes) {
    nil_expr: NilExpression,
    boolean_expr: BooleanExpression,
    number_expr: NumberExpression,
    string_expr: StringExpression,
    identifier_expr: Identifier,
    infix_expr: InfixExpression,
    prefix_expr: PrefixExpression,
    if_expr: IfExpression,
    fn_expr: fnExpression,
    call_expr: callExpression,
    array_expr: ArrayExpression,
    index_expr: IndexExpression,
};

pub const NodeTypes = enum {
    statement,
    expression,
};

pub const CurrentNode = union(NodeTypes) {
    statement: *Statement,
    expression: *Expression,
};

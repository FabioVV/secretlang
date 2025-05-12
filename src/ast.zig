const std = @import("std");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

pub const Program = struct {
    nodes: std.ArrayList(Node),

    pub fn init() !Program {
        const program = Program{
            .nodes = std.ArrayList(Node).init(std.heap.page_allocator),
        };
        return program;
    }

    pub fn addNode(self: *Program, node: Node) !void {
        try self.nodes.append(node);
    }

    pub fn deinit(self: *Program) void {
        self.nodes.deinit();
    }
};

pub const Node = union(enum) {
    var_stmt: VarStatement,
    r_stmt: ReturnStatement,
    e_stmt: ExpressionStatement,
};

pub const Identifier = struct {
    token: Token, // IDENT token.
    literal: []const u8,
};
pub const Expression = struct {
    node: Node,
};

pub const VarStatement = struct {
    token: Token, // VAR token.
    identifier: Identifier = undefined,
    expression: Expression = undefined, // the value being assigned to the var variable
};

pub const ReturnStatement = struct {
    token: Token, // RETURN token
    expresssion: Expression = undefined, // the return value
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression = undefined,
};

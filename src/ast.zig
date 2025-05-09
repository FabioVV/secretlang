const std = @import("std");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

const Program = struct {
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

const Node = union(enum) {
    var_stmt: VarStatement,
};

const Identifier = struct {
    token: Token, // IDENT token.
    literal: []const u8,
};
const Expression = struct {};

const VarStatement = struct {
    token: Token, // VAR token.
    identifier: Identifier,
    value: Expression, // the value being assigned to the var variable
};

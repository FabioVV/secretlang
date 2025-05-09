const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

const Program = struct {
    nodes: []Node,
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

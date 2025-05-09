const std = @import("std");
const mem = std.mem;
const proc = std.process;
const expect = std.testing.expect;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Lexer = @import("lexer.zig").Lexer;
const AST = @import("ast.zig");

const Parser = struct {
    lexer: *Lexer,
    cur_token: Token = undefined,
    peek_token: Token = undefined,

    pub fn init(lexer: *Lexer) !Parser {
        var parser: Parser = Parser{ .lexer = lexer };

        // Set both peek token and cur token
        parser.cur_token = parser.peek_token;
        parser.peek_token = try parser.lexer.nextToken();
        return parser;
    }

    pub fn nextToken(parser: *Parser) void {
        parser.cur_token = parser.peek_token;
        parser.peek_token = try parser.lexer.nextToken();
    }

    pub fn parseProgram(parser: *Parser) *AST.Program {
        const program: AST.program = try AST.Program.init();

        while (parser.peek_token.token_type != Tokens.EOF) {
            const node = parser.parseNode();
            if (node != null) {
                program.addNode(node);
            }
            parser.nextToken();
        }

        return &program;
    }

    pub fn parserVarToken(parser: *Parser) ?AST.VarStatement {
        const vstmt = AST.VarStatement{ .token = parser.cur_token };

        if (!parser.expectNext(Tokens.IDENT)) {
            return null;
        }

        vstmt.identifier = AST.Identifier{ .token = parser.cur_token, .literal = parser.cur_token.literal };

        if (!parser.expectNext(Tokens.EQUAL)) {
            return null;
        }

        return vstmt;
    }

    pub fn parseNode(parser: *Parser) ?AST.Node {
        switch (parser.cur_token.token_type) {
            Tokens.VAR => {
                const var_stmt = parser.parserVarToken();
                return AST.Node{ .var_stmt = var_stmt };
            },
            else => {
                return null;
            },
        }
    }
};

test "Parser initializtion" {
    const input: []const u8 =
        \\var name = "Fábio Gabriel Rodrigues Varela"
    ;

    var l: Lexer = try Lexer.init(input);
    const p: Parser = try Parser.init(&l);
    try expect(mem.eql(u8, l.content, p.lexer.content));
}

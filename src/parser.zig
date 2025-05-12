const std = @import("std");
const mem = std.mem;
const proc = std.process;
const expect = std.testing.expect;

const debug = @import("debug.zig");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Lexer = @import("lexer.zig").Lexer;
const AST = @import("ast.zig");

const Precedence = enum(u32) {
    DEFAULT = 0,
    EQUALS = 1,
    LESS_GREATER = 2,
    CONDITIONAL = 3,
    SUM = 4,
    PRODUCT = 5,
    EXPONENT = 6,
    PREFIX = 7,
    POSTFIX = 8,
    CALL = 9,
};

const NudParseFn = *const fn (*Parser) AST.Expression;
const LedParseFn = *const fn (*Parser, AST.Expression) AST.Expression;

pub fn parseIdentifier(p: *Parser) AST.Expression {
    return (AST.Expression{ .identifier_expr = AST.Identifier{ .token = p.cur_token, .literal = p.cur_token.literal } });
}

pub fn parseNumber(p: *Parser) AST.Expression {
    var number = AST.NumberExpression{ .token = p.cur_token };

    const result = std.fmt.parseFloat(f64, p.cur_token.literal) catch unreachable;
    number.value = result;

    //std.debug.print("{d}\n", .{number.value});

    return (AST.Expression{ .number_expr = number });
}

pub const Parser = struct {
    lexer: *Lexer,
    cur_token: Token = undefined,
    peek_token: Token = undefined,
    nud_handlers: std.AutoHashMap(Tokens, NudParseFn) = undefined,
    led_handlers: std.AutoHashMap(Tokens, LedParseFn) = undefined,
    binding_powers: std.AutoHashMap(Tokens, Precedence) = undefined,

    pub fn init(lexer: *Lexer) !Parser {
        var parser: Parser = Parser{ .lexer = lexer };

        parser.nud_handlers = std.AutoHashMap(Tokens, NudParseFn).init(std.heap.page_allocator);
        parser.led_handlers = std.AutoHashMap(Tokens, LedParseFn).init(std.heap.page_allocator);
        parser.binding_powers = std.AutoHashMap(Tokens, Precedence).init(std.heap.page_allocator);

        // Setting up the binding power hashtable
        try parser.binding_powers.put(Tokens.EQUAL, Precedence.EQUALS);
        try parser.binding_powers.put(Tokens.NOT_EQUAL, Precedence.EQUALS);
        try parser.binding_powers.put(Tokens.LESST, Precedence.LESS_GREATER);
        try parser.binding_powers.put(Tokens.GREATERT, Precedence.LESS_GREATER);
        try parser.binding_powers.put(Tokens.PLUS, Precedence.SUM);
        try parser.binding_powers.put(Tokens.MINUS, Precedence.SUM);
        try parser.binding_powers.put(Tokens.FSLASH, Precedence.PRODUCT);
        try parser.binding_powers.put(Tokens.ASTERISK, Precedence.PRODUCT);

        // Setting up the parsing fns hashtable
        try parser.nud_handlers.put(Tokens.IDENT, parseIdentifier);
        try parser.nud_handlers.put(Tokens.NUMBER, parseNumber);
        return parser;
    }

    pub fn advance(parser: *Parser) !void {
        parser.cur_token = parser.peek_token;
        parser.peek_token = try parser.lexer.nextToken();
    }

    pub fn peekBindingPower(parser: *Parser) Precedence {
        return parser.binding_powers.get(parser.cur_token.token_type) orelse Precedence.DEFAULT;
    }

    pub inline fn currentIs(parser: *Parser, token: Token) bool {
        return parser.cur_token.token_type == token.token_type;
    }

    pub inline fn peekIs(parser: *Parser, token_type: Tokens) bool {
        return parser.peek_token.token_type == token_type;
    }

    pub fn expect(parser: *Parser, token_type: Tokens) bool {
        if (parser.peekIs(token_type)) {
            parser.advance() catch |err| {
                std.debug.print("Error getting next token on parser: {any}", .{err});
            };
            return true;
        } else {
            return false;
        }
    }

    pub fn parseProgram(p: *Parser) !*AST.Program {
        var program: AST.Program = try AST.Program.init();

        try p.advance();
        try p.advance();

        while (p.peek_token.token_type != Tokens.EOF) {
            const node = p.parseNode();
            if (node != null) {
                try program.addNode(node.?);
            }
            try p.advance();
        }

        return &program;
    }

    pub fn parseVarToken(parser: *Parser) ?AST.VarStatement {
        var vstmt = AST.VarStatement{ .token = parser.cur_token };

        if (!parser.expect(Tokens.IDENT)) {
            return null;
        }

        vstmt.identifier = AST.Identifier{ .token = parser.cur_token, .literal = parser.cur_token.literal };

        if (!parser.expect(Tokens.EQUAL)) {
            return null;
        }

        parser.advance() catch |err| {
            std.debug.print("Error getting next token on parser: {any}", .{err});
        };

        vstmt.expression = parser.parseExpression(Precedence.DEFAULT).?;

        return vstmt;
    }

    pub fn parseReturnToken(parser: *Parser) ?AST.ReturnStatement {
        const rstmt = AST.ReturnStatement{ .token = parser.cur_token };

        parser.advance() catch |err| {
            std.debug.print("Error getting next token on parser: {any}", .{err});
        };

        return rstmt;
    }

    pub fn parseExpressionStatement(parser: *Parser) ?AST.ExpressionStatement {
        var estmt = AST.ExpressionStatement{ .token = parser.cur_token };

        const expression = parser.parseExpression(Precedence.DEFAULT);
        if (expression == null) {
            return null;
        }
        estmt.expression = expression.?;

        return estmt;
    }

    pub fn parseExpression(parser: *Parser, prec: Precedence) ?*AST.Expression {
        // Retrieve the prefix parse function for the current token
        const prefix_fn = parser.nud_handlers.get(parser.cur_token.token_type) orelse return null;

        // Call the prefix function to parse the left-hand side
        var left = prefix_fn(@constCast(parser));

        // Loop to handle infix operators
        while (@intFromEnum(prec) < @intFromEnum(parser.peekBindingPower())) {
            const infix_fn = parser.led_handlers.get(parser.peek_token.token_type) orelse return &left;

            // Advance to the next token
            parser.advance() catch |err| {
                std.debug.print("Error getting next token on parser: {any}\n", .{err});
                return null; // Handle the error appropriately
            };

            // Call the infix function to parse the right-hand side
            left = infix_fn(@constCast(parser), left);
        }

        return &left;
    }

    pub fn parseNode(parser: *Parser) ?AST.Statement {
        return switch (parser.cur_token.token_type) {
            Tokens.VAR => {
                const var_stmt = parser.parseVarToken();
                if (var_stmt == null) {
                    return null;
                }
                return AST.Statement{ .var_stmt = var_stmt.? };
            },
            Tokens.RETURN => {
                const r_stmt = parser.parseReturnToken();
                if (r_stmt == null) {
                    return null;
                }
                return AST.Statement{ .r_stmt = r_stmt.? };
            },
            else => {
                const e_stmt = parser.parseExpressionStatement();
                if (e_stmt == null) {
                    return null;
                }
                return AST.Statement{ .e_stmt = e_stmt.? };
            },
        };
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

test "Var statement parsing" {
    const input: []const u8 =
        \\var age = 22
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l);
    const program: *AST.Program = try p.parseProgram();

    for (program.nodes.items) |node| {
        std.debug.print("{any}\n\n", .{node});
        // debug.printVarStatement(node.var_stmt);
    }
}

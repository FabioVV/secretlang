const std = @import("std");
const mem = std.mem;
const proc = std.process;
const expect = std.testing.expect;

const errorHandling = @import("error.zig");
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

const NudParseFn = *const fn (*Parser) *AST.Expression;
const LedParseFn = *const fn (*Parser, *AST.Expression) *AST.Expression;

pub const Parser = struct {
    lexer: *Lexer,
    cur_token: Token = undefined,
    peek_token: Token = undefined,
    nud_handlers: std.AutoHashMap(Tokens, NudParseFn) = undefined,
    led_handlers: std.AutoHashMap(Tokens, LedParseFn) = undefined,
    binding_powers: std.AutoHashMap(Tokens, Precedence) = undefined,
    arena: std.heap.ArenaAllocator,

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) !Parser {
        var parser: Parser = Parser{ .lexer = lexer, .arena = std.heap.ArenaAllocator.init(allocator) };

        // Initializing hashmaps
        parser.nud_handlers = std.AutoHashMap(Tokens, NudParseFn).init(std.heap.page_allocator);
        parser.led_handlers = std.AutoHashMap(Tokens, LedParseFn).init(std.heap.page_allocator);
        parser.binding_powers = std.AutoHashMap(Tokens, Precedence).init(std.heap.page_allocator);

        // Setting up the binding power hashtable
        parser.bindingPower(Tokens.EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.NOT_EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.LESST, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.GREATERT, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.PLUS, Precedence.SUM);
        parser.bindingPower(Tokens.MINUS, Precedence.SUM);
        parser.bindingPower(Tokens.FSLASH, Precedence.PRODUCT);
        parser.bindingPower(Tokens.ASTERISK, Precedence.PRODUCT);

        // Setting up the parsing functions
        parser.nud(Tokens.IDENT, parseIdentifier);
        parser.nud(Tokens.NUMBER, parseNumber);

        parser.nud(Tokens.NOT, parseNud);
        parser.nud(Tokens.MINUS, parseNud);

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
        self.nud_handlers.deinit();
        self.led_handlers.deinit();
        self.binding_powers.deinit();
    }

    pub inline fn bindingPower(self: *Parser, token_type: Tokens, prec: Precedence) void {
        self.binding_powers.put(token_type, prec) catch |err| {
            errorHandling.exitWithError("Error registering binding power", err);
        };
    }

    pub inline fn nud(self: *Parser, token_type: Tokens, func: NudParseFn) void {
        self.nud_handlers.put(token_type, func) catch |err| {
            errorHandling.exitWithError("Error registering nud(prefix) parse function", err);
        };
    }

    pub inline fn led(self: *Parser, token_type: Tokens, func: NudParseFn) void {
        self.led_handlers.put(token_type, func) catch |err| {
            errorHandling.exitWithError("Error registering led(infix) parse function", err);
        };
    }

    pub fn parseNud(self: *Parser) *AST.Expression {
        const cur_token = self.cur_token;

        self.advance() catch |err| {
            std.debug.print("Error getting next token on parser: {any}", .{err});
        };

        const rightExpression = self.parseExpression(Precedence.PREFIX).?; // HANDLE THIS BETTER

        const prefixExpr = AST.PrefixExpression{ .token = cur_token, .right = rightExpression };

        const expr = self.createExpressionNode().?; // HANDLE THIS BETTER
        expr.* = AST.Expression{ .prefix_expr = prefixExpr };

        return expr;
    }

    pub fn parseIdentifier(self: *Parser) *AST.Expression {
        const expr = self.createExpressionNode().?;
        expr.* = AST.Expression{ .identifier_expr = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal } };
        return expr;
    }

    pub fn parseNumber(self: *Parser) *AST.Expression {
        const num_token = self.cur_token;

        const result = std.fmt.parseFloat(f64, self.cur_token.literal) catch unreachable;
        const num_exp = AST.NumberExpression{ .token = num_token, .value = result };

        const expr = self.createExpressionNode().?; // HANDLE THIS BETTER
        expr.* = AST.Expression{ .number_expr = num_exp };

        return expr;
    }

    pub fn createExpressionNode(self: *Parser) ?*AST.Expression {
        const expr = self.arena.allocator().create(AST.Expression) catch |err| {
            std.debug.print("Error when trying to create AST expression nodes: {any}\n", .{err});
            return null;
        };

        return expr;
    }

    pub inline fn advance(parser: *Parser) !void {
        parser.cur_token = parser.peek_token;
        parser.peek_token = try parser.lexer.nextToken();
    }

    pub inline fn peekBindingPower(parser: *Parser) Precedence {
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

    pub fn parseProgram(p: *Parser, allocator: std.mem.Allocator) !?*AST.Program {
        const program = AST.Program.init(allocator) orelse return null;

        try p.advance();
        try p.advance();

        while (p.peek_token.token_type != Tokens.EOF) {
            const node = p.parseNode();
            if (node != null) {
                try program.*.addNode(node.?);
            }
            try p.advance();
        }

        return program;
    }

    pub fn parseVarToken(parser: *Parser) ?AST.VarStatement {
        const var_token = parser.cur_token;

        if (!parser.expect(Tokens.IDENT)) {
            return null;
        }

        const identifier = AST.Identifier{ .token = parser.cur_token, .literal = parser.cur_token.literal };

        if (!parser.expect(Tokens.EQUAL)) {
            return null;
        }

        parser.advance() catch |err| {
            std.debug.print("Error getting next token on parser: {any}", .{err});
        };

        const expression = parser.parseExpression(Precedence.DEFAULT).?;

        return AST.VarStatement{ .token = var_token, .identifier = identifier, .expression = expression };
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
            const infix_fn = parser.led_handlers.get(parser.peek_token.token_type) orelse return left;

            // Advance to the next token
            parser.advance() catch |err| {
                std.debug.print("Error getting next token on parser: {any}\n", .{err});
                return null; // Handle the error appropriately
            };

            // Call the infix function to parse the right-hand side
            left = infix_fn(@constCast(parser), left);
        }

        return left;
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
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    try expect(mem.eql(u8, l.content, p.lexer.content));
}

test "Var statement parsing" {
    const input: []const u8 =
        \\var age = 22
        \\var num = 50
        \\var num_frac = 50.50
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        //return null;
        try expect(false);
    }

    defer program.?.deinit();

    for (program.?.nodes.items) |node| {
        debug.printVarStatement(node.var_stmt);
    }
}

test "Prefix parsing" {
    const input: []const u8 =
        \\!5
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        //return null;
        try expect(false);
    }

    defer program.?.deinit();

    for (program.?.nodes.items) |node| {
        debug.printPrefixExpression(node.e_stmt.expression.*.prefix_expr);
    }
}

const std = @import("std");
const mem = std.mem;
const proc = std.process;
const expect = std.testing.expect;

const errorHandling = @import("error.zig");
const debug = @import("debug.zig");
const _token = @import("token.zig");
const Position = _token.Position;
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
    INDEX = 10,
};

const SyncTokens = &[_]Tokens{ .EOF, .VAR, .RBRACE, .RETURN }; // Tokens that can be used as a stop point to syncronize the parser if it encounters an error

const NudParseFn = *const fn (*Parser) ?*AST.Expression;
const LedParseFn = *const fn (*Parser, ?*AST.Expression) ?*AST.Expression;

pub const ParserError = struct {
    message: []const u8,
};

pub const Parser = struct {
    lexer: *Lexer,
    cur_token: Token = undefined,
    peek_token: Token = undefined,
    nud_handlers: std.AutoHashMap(Tokens, NudParseFn) = undefined,
    led_handlers: std.AutoHashMap(Tokens, LedParseFn) = undefined,
    binding_powers: std.AutoHashMap(Tokens, Precedence) = undefined,
    errors: std.ArrayList(ParserError),
    arena: std.heap.ArenaAllocator,

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        var parser: Parser = Parser{ .lexer = lexer, .arena = std.heap.ArenaAllocator.init(allocator), .errors = std.ArrayList(ParserError).init(allocator) };

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
        parser.bindingPower(Tokens.LPAREN, Precedence.CALL);
        parser.bindingPower(Tokens.LBRACKET, Precedence.INDEX);


        // Setting up the parsing functions
        parser.nud(Tokens.IDENT, parseIdentifier);
        parser.nud(Tokens.NUMBER, parseNumber);
        parser.nud(Tokens.TRUE, parseBoolean);
        parser.nud(Tokens.FALSE, parseBoolean);
        parser.nud(Tokens.NOT, parseNud);
        parser.nud(Tokens.MINUS, parseNud);
        parser.nud(Tokens.LPAREN, parseGroupExpression);
        parser.nud(Tokens.IF, parseIfExpression);
        parser.nud(Tokens.FN, parseFnExpression);


        parser.led(Tokens.PLUS, parseLed);
        parser.led(Tokens.MINUS, parseLed);
        parser.led(Tokens.FSLASH, parseLed);
        parser.led(Tokens.ASTERISK, parseLed);
        parser.led(Tokens.EQUAL, parseLed);
        parser.led(Tokens.NOT_EQUAL, parseLed);
        parser.led(Tokens.LESST, parseLed);
        parser.led(Tokens.GREATERT, parseLed);
        parser.led(Tokens.LPAREN, parseCallExpression);

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
        self.nud_handlers.deinit();
        self.led_handlers.deinit();
        self.binding_powers.deinit();

        for (self.errors.items) |err| {
            std.heap.page_allocator.free(err.message);
        }

        self.errors.deinit();
    }

    pub fn currentError(self: *Parser, token_literal: []const u8) void {
        const token = self.cur_token;

        const msg = std.fmt.allocPrint(std.heap.page_allocator, "error: line {d} column {d}: expected {s} but got {s} instead\n", .{ token.position.line, token.position.column, token_literal, token.literal }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        self.errors.append(ParserError{ .message = std.heap.page_allocator.dupe(u8, msg) catch |_err| {
            errorHandling.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    pub fn peekError(self: *Parser, token_literal: []const u8) void {
        const token = self.peek_token;

        const msg = std.fmt.allocPrint(std.heap.page_allocator, "error: line {d} column {d}: expected {s} but got {s} instead\n", .{ token.position.line, token.position.column, token_literal, token.literal }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        self.errors.append(ParserError{ .message = std.heap.page_allocator.dupe(u8, msg) catch |_err| {
            errorHandling.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    pub fn sync(self: *Parser) void {
        while (self.cur_token.token_type != Tokens.EOF) {
            for (SyncTokens) |st| {
                if (st == self.cur_token.token_type) {
                    return;
                }
            }
            self.advance();
        }
    }

    pub fn curTokenParseError(self: *Parser, errorMessage: []const u8) void {
        const token = self.cur_token;

        const msg = std.fmt.allocPrint(std.heap.page_allocator, "error: line {d} column {d}: {s}\n", .{ token.position.line, token.position.column, errorMessage }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        self.errors.append(ParserError{ .message = std.heap.page_allocator.dupe(u8, msg) catch |_err| {
            errorHandling.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    //pub fn noNudHandlerError(self: *Parser, token_literal: []const u8) void {}

    //pub fn noLedHandlerError(self: *Parser, token_literal: []const u8) void {}

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

    pub inline fn led(self: *Parser, token_type: Tokens, func: LedParseFn) void {
        self.led_handlers.put(token_type, func) catch |err| {
            errorHandling.exitWithError("Error registering led(infix) parse function", err);
        };
    }

    pub fn parseNud(self: *Parser) ?*AST.Expression {
        const cur_token = self.cur_token;

        self.advance();

        if (!self.expectCurrentTokenIs(Tokens.NUMBER, "number")) {
            return null;
        }

        const rightExpression = self.parseExpression(Precedence.PREFIX) orelse null; // HANDLE THIS BETTER

        const prefixExpr = AST.PrefixExpression{ .token = cur_token, .right = rightExpression };

        const expr = self.createExpressionNode(); // HANDLE THIS BETTER
        expr.* = AST.Expression{ .prefix_expr = prefixExpr };

        return expr;
    }

    pub fn parseLed(self: *Parser, left_expr: ?*AST.Expression) ?*AST.Expression {
        const cur_token = self.cur_token;
        const prec = self.curBindingPower();

        var infixExpr = AST.InfixExpression{ .token = cur_token, .left = left_expr };

        self.advance();

        if (!self.expectCurrentTokenIs(Tokens.NUMBER, "number")) {
            return null;
        }

        const rightExpression = self.parseExpression(prec) orelse null;
        infixExpr.right = rightExpression;

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .infix_expr = infixExpr };

        return expr;
    }

    pub fn parseCallArgs(self: *Parser) ?[]?*AST.Expression{
        var args = std.BoundedArray(?*AST.Expression, 32).init(0) catch unreachable;

        if (self.peekIs(Tokens.RPAREN)) {
            self.advance();

            return &[_]?*AST.Expression{};
        }


        self.advance();

        const arg = self.parseExpression(Precedence.DEFAULT) orelse null;

        args.append(arg) catch |err| {
            errorHandling.exitWithError("Unrecoverable error when trying to append function argument.", err);
        };

        while(self.peekIs(Tokens.COMMA)){
            self.advance();  // advance after the comma
            self.advance(); // advance again, this time cur_token becomes the next argument


            const _arg = self.parseExpression(Precedence.DEFAULT) orelse null;
            args.append(_arg) catch |err| {
                errorHandling.exitWithError("Unrecoverable error when trying to append function argument.", err);
            };
        }

        if (!self.expect(Tokens.RPAREN, ")")) {
            return null;
        }

        return args.slice();
    }

    pub fn parseCallExpression(self: *Parser, fn_expr: ?*AST.Expression) ?*AST.Expression {
        const expr = self.createExpressionNode();
        var callExpr = AST.callExpression.init(self.cur_token, fn_expr);

        const args = self.parseCallArgs();
        if(args != null){
            callExpr.arguments.appendSlice(args.?) catch unreachable;
        }

        expr.* = AST.Expression{ .call_expr = callExpr };

        return expr;
    }

    pub fn parseGroupExpression(self: *Parser) ?*AST.Expression {
        self.advance();

        const expression = self.parseExpression(Precedence.DEFAULT);

        if (expression == null) {
            self.curTokenParseError("expected an expression but got EOF instead");
            return null;
        }

        const expr = self.createExpressionNode();
        expr.* = expression.?.*;

        if (!self.expect(Tokens.RPAREN, ")")) {
            return expr;
        }

        return expr;
    }

    pub fn parseBlockStatement(self: *Parser) ?AST.BlockStatement {
        var block = AST.BlockStatement.init(std.heap.page_allocator, self.cur_token);

        self.advance();

        while (!self.currentIs(Tokens.RBRACE)) {
            const node_stmt = self.parseNode();

            if (node_stmt != null) {
                block.statements.append(node_stmt.?) catch |err| {
                    std.debug.print("Error appeding statement node in IF expression: {any}", .{err});
                };
            }


            self.advance();

            if (self.currentIs(Tokens.EOF)) {
                self.curTokenParseError("expected } but got EOF instead");
                return null;
            }
        }

        return block;
    }

    pub fn parseIfExpression(self: *Parser) ?*AST.Expression {
        const if_token = self.cur_token;

        if (!self.expect(Tokens.LPAREN, "(")) {
            return null;
        }

        self.advance();

        const expression_condition = self.parseExpression(Precedence.DEFAULT);
        if (expression_condition == null) {
            self.curTokenParseError("expected an expression but got EOF instead");
            return null;
        }

        var if_exp = AST.IfExpression{ .token = if_token, .condition = expression_condition };

        if (!self.expect(Tokens.RPAREN, ")")) {
            return null;
        }

        if (!self.expect(Tokens.LBRACE, "{")) {
            return null;
        }

        if_exp.ifBlock = self.parseBlockStatement() orelse null;


        if (self.peekIs(Tokens.ELSE)) {
            self.advance();

            if (!self.expect(Tokens.LBRACE, "{")) {
                return null;
            }

            if_exp.elseBlock = self.parseBlockStatement() orelse null;
        }

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .if_expr = if_exp };

        return expr;
    }

    pub fn parseFnParameters(self: *Parser) ?[]AST.Identifier {
        var params = std.BoundedArray(AST.Identifier, 32).init(0) catch unreachable;

        if(self.peekIs(Tokens.RPAREN)){
            self.advance();

            return &[_]AST.Identifier{};
        }

        self.advance();

        const ident = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal };
        params.append(ident) catch |err| {
            errorHandling.exitWithError("Unrecoverable error when trying to append function parameter.", err);
        };

        while(self.peekIs(Tokens.COMMA)){
            self.advance();  // advance after the comma
            self.advance(); // advance again, this time cur_token becomes the next parameter

            const _ident = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal };
            params.append(_ident) catch |err| {
                errorHandling.exitWithError("Unrecoverable error when trying to append function parameter.", err);
            };
        }

        if (!self.expect(Tokens.RPAREN, ")")) {
            return null;
        }

        return params.slice();
    }

    pub fn parseFnExpression(self: *Parser) ?*AST.Expression {
        var fnLiteral = AST.fnExpression.init(self.cur_token);

        if (!self.expect(Tokens.LPAREN, "(")) {
            return null;
        }

        const params = self.parseFnParameters();

        if(params != null){
            fnLiteral.parameters.appendSlice(params.?) catch |err| {
                errorHandling.exitWithError("Unrecoverable error when trying to append slice of function parameters.", err);
            };
        }

        if (!self.expect(Tokens.LBRACE, "{")) {
            return null;
        }

        fnLiteral.body = self.parseBlockStatement() orelse null;

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .fn_expr =  fnLiteral };

        return expr;
    }

    pub fn parseIdentifier(self: *Parser) ?*AST.Expression {
        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .identifier_expr = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal } };
        return expr;
    }

    pub fn parseNumber(self: *Parser) ?*AST.Expression {
        const num_token = self.cur_token;

        const result = std.fmt.parseFloat(f64, self.cur_token.literal) catch unreachable;
        const num_exp = AST.NumberExpression{ .token = num_token, .value = result };

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .number_expr = num_exp };

        return expr;
    }

    pub fn parseBoolean(self: *Parser) ?*AST.Expression {
        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .boolean_expr = AST.BooleanExpression{ .token = self.cur_token, .value = self.currentIs(Tokens.TRUE) } };
        return expr;
    }

    pub fn createExpressionNode(self: *Parser) *AST.Expression {
        const expr = self.arena.allocator().create(AST.Expression) catch |err| {
            errorHandling.exitWithError("Unrecoverable error when trying to create expression node.", err);
        };

        return expr;
    }

    pub inline fn advance(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken() catch |err| {
            errorHandling.exitWithError("Unrecoverable error when trying to advance tokens.", err);
        };
    }

    pub inline fn peekBindingPower(self: *Parser) Precedence {
        return self.binding_powers.get(self.peek_token.token_type) orelse Precedence.DEFAULT;
    }

    pub inline fn curBindingPower(self: *Parser) Precedence {
        return self.binding_powers.get(self.cur_token.token_type) orelse Precedence.DEFAULT;
    }

    pub inline fn currentIs(self: *Parser, token_type: Tokens) bool {
        return self.cur_token.token_type == token_type;
    }

    pub inline fn peekIs(self: *Parser, token_type: Tokens) bool {
        return self.peek_token.token_type == token_type;
    }

    pub fn expect(self: *Parser, token_type: Tokens, token_literal: []const u8) bool {
        if (self.peekIs(token_type)) {
            self.advance();
            return true;
        } else {
            self.peekError(token_literal);
            self.sync();
            return false;
        }
    }
    /// Checks if the current token is of the same type as the token_type param.
    /// Need a better version of this, need to check for more types (maybe create a list of accetable types based on what is being parsed?)
    pub fn expectCurrentTokenIs(self: *Parser, token_type: Tokens, token_literal: []const u8) bool {
        if (self.currentIs(token_type)) {
            return true;
        } else {
            self.currentError(token_literal); // fix this
            self.sync();
            return false;
        }
    }

    pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) !?*AST.Program {
        const program = AST.Program.init(allocator) orelse return null;

        self.advance();
        self.advance();

        while (self.cur_token.token_type != Tokens.EOF) {
            const node = self.parseNode();

            if (node != null) {
                try program.*.addNode(node.?);
            }

            self.advance();
        }

        return program;
    }

    pub fn parseVarToken(self: *Parser) ?AST.VarStatement {
        const var_token = self.cur_token;

        if (!self.expect(Tokens.IDENT, "identifier")) {
            return null;
        }

        const identifier = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal };

        if (!self.expect(Tokens.EQUAL, "=")) {
            return null;
        }

        self.advance();

        const expression = self.parseExpression(Precedence.DEFAULT);
        if (expression == null) {
            self.curTokenParseError("expected an expression but got EOF instead");
            return null;
        }

        return AST.VarStatement{ .token = var_token, .identifier = identifier, .expression = expression.? };
    }

    pub fn parseReturnToken(self: *Parser) ?AST.ReturnStatement {
        var rstmt = AST.ReturnStatement{ .token = self.cur_token };
        self.advance();

        const expression = self.parseExpression(Precedence.DEFAULT);
        if (expression == null) {
            self.curTokenParseError("expected an expression but got EOF instead");
            return null;
        }

        rstmt.expression = expression;

        return rstmt;
    }

    pub fn parseExpressionStatement(self: *Parser) ?AST.ExpressionStatement {
        var estmt = AST.ExpressionStatement{ .token = self.cur_token };

        const expression = self.parseExpression(Precedence.DEFAULT);
        if (expression == null) {
            //self.curTokenParseError("expected an expression but got EOF instead");
            return null;
        }

        estmt.expression = expression.?;

        return estmt;
    }

    pub fn parseExpression(self: *Parser, prec: Precedence) ?*AST.Expression {
        const prefix_fn = self.nud_handlers.get(self.cur_token.token_type) orelse return null;

        // call the prefix function to parse the left-hand side
        var left = prefix_fn(@constCast(self));

        // infix stuff
        while (@intFromEnum(prec) < @intFromEnum(self.peekBindingPower())) {
            const infix_fn = self.led_handlers.get(self.peek_token.token_type) orelse return left;

            self.advance();

            // call the infix function to parse the right-hand side
            left = infix_fn(@constCast(self), left orelse null);
        }

        return left;
    }

    pub fn parseNode(self: *Parser) ?AST.Statement {
        return switch (self.cur_token.token_type) {
            Tokens.VAR => {
                const var_stmt = self.parseVarToken();
                if (var_stmt == null) {
                    return null;
                }
                return AST.Statement{ .var_stmt = var_stmt.? };
            },
            Tokens.RETURN => {
                const r_stmt = self.parseReturnToken();
                if (r_stmt == null) {
                    return null;
                }
                return AST.Statement{ .return_stmt = r_stmt.? };
            },
            else => {
                const e_stmt = self.parseExpressionStatement();
                if (e_stmt == null) {
                    return null;
                }
                return AST.Statement{ .expression_stmt = e_stmt.? };
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

    try expect(p.errors.items.len == 0);

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

    try expect(p.errors.items.len == 0);


    for (program.?.nodes.items) |node| {
        debug.printVarStatement(node.var_stmt);
    }
}

test "Var statement parsing errors len" {
    const input: []const u8 =
        \\var b 15
        \\var a 55
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len > 0);

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

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        debug.printPrefixExpression(node.expression_stmt.expression.?.*.prefix_expr);
    }
}

test "simple if parsing" {
    const input: []const u8 =
        \\if(true){
        \\ var age = 22
        \\}
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
    try expect(p.errors.items.len == 0);

    //for (program.?.nodes.items) |node| {
    //    debug.printIfExpression(node.expression_stmt.expression.?.*.if_expr);
    //}
}

test "if parsing with else" {
    const input: []const u8 =
        \\if(true){
        \\ var els = true
        \\} else {
        \\ var els = false
        \\}
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        debug.printIfExpression(node.expression_stmt.expression.?.*.if_expr);
    }
}

test "function literal" {
    const input: []const u8 =
    \\fn(){}
    \\fn(a){}
    \\fn(a, b){}
    \\fn(a, b, c){}
    \\fn(a, b, c, d){
    \\ var a = 55;
    \\}
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        debug.printFnExpression(node.expression_stmt.expression.?.*.fn_expr);
    }
}

test "function call" {
    const input: []const u8 =
    \\add()
    \\add(1, 2)
    \\add(1, 2, 3)
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = try Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);


    for (program.?.nodes.items) |node| {
        debug.printFnExpressionCall(node.expression_stmt.expression.?.*.call_expr);
    }
}

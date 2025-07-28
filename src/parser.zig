const std = @import("std");
const mem = std.mem;
const proc = std.process;
const expect = std.testing.expect;

const errh = @import("error.zig");
const dbg = @import("debug.zig");
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


const NudParseFn = *const fn (*Parser) ?*AST.Expression;
const LedParseFn = *const fn (*Parser, ?*AST.Expression) ?*AST.Expression;

const ParserError = struct {
    message: []const u8,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    lexer: *Lexer,

    previous_token: Token,
    cur_token: Token,
    peek_token: Token,

    nud_handlers: std.AutoHashMap(Tokens, NudParseFn),
    led_handlers: std.AutoHashMap(Tokens, LedParseFn),
    binding_powers: std.AutoHashMap(Tokens, Precedence),

    had_error: bool,
    in_panic: bool,
    errors: std.ArrayList(ParserError),

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) *Parser {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const parser = arena.allocator().create(Parser) catch unreachable;

        parser.arena = arena;
        parser.allocator = parser.arena.?.allocator();
        parser.lexer = lexer;
        parser.in_panic = false;
        parser.had_error = false;
        parser.previous_token = undefined;
        parser.cur_token = undefined;
        parser.peek_token = undefined;

        parser.errors = std.ArrayList(ParserError).init(parser.allocator);

        // Initializing hashmaps
        parser.nud_handlers = std.AutoHashMap(Tokens, NudParseFn).init(parser.allocator);
        parser.led_handlers = std.AutoHashMap(Tokens, LedParseFn).init(parser.allocator);
        parser.binding_powers = std.AutoHashMap(Tokens, Precedence).init(parser.allocator);

        // Setting up the binding power hashtable
        parser.bindingPower(Tokens.EQUAL_EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.NOT_EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.LESST, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.GREATERT, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.LESS_EQUAL, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.GREATER_EQUAL, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.PLUS, Precedence.SUM);
        parser.bindingPower(Tokens.MINUS, Precedence.SUM);
        parser.bindingPower(Tokens.FSLASH, Precedence.PRODUCT);
        parser.bindingPower(Tokens.ASTERISK, Precedence.PRODUCT);
        parser.bindingPower(Tokens.LPAREN, Precedence.CALL);
        parser.bindingPower(Tokens.LBRACKET, Precedence.INDEX);

        // This precedences here might be 'wrong', need to test more later
        parser.bindingPower(Tokens.MODULO, Precedence.PRODUCT);
        parser.bindingPower(Tokens.BIT_AND, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.LEFT_SHIFT, Precedence.SUM);
        parser.bindingPower(Tokens.RIGHT_SHIFT, Precedence.SUM);
        parser.bindingPower(Tokens.PIPE, Precedence.EQUALS);

        // Setting up the parsing functions
        parser.nud(Tokens.IDENT, parseIdentifier);
        parser.nud(Tokens.NUMBER, parseNumber);
        parser.nud(Tokens.TRUE, parseBoolean);
        parser.nud(Tokens.FALSE, parseBoolean);
        parser.nud(Tokens.NIL, parseNil);
        parser.nud(Tokens.STRING, parseString);
        parser.nud(Tokens.NOT, parseNud);
        parser.nud(Tokens.MINUS, parseNud);
        parser.nud(Tokens.LPAREN, parseGroupExpression);
        parser.nud(Tokens.IF, parseIfExpression);
        parser.nud(Tokens.FN, parseFnExpression);
        parser.nud(Tokens.LBRACKET, parseArrayExpression);

        parser.led(Tokens.PLUS, parseLed);
        parser.led(Tokens.MINUS, parseLed);
        parser.led(Tokens.FSLASH, parseLed);
        parser.led(Tokens.ASTERISK, parseLed);
        parser.led(Tokens.EQUAL_EQUAL, parseLed);
        parser.led(Tokens.NOT_EQUAL, parseLed);
        parser.led(Tokens.LESST, parseLed);
        parser.led(Tokens.GREATERT, parseLed);
        parser.led(Tokens.LESS_EQUAL, parseLed);
        parser.led(Tokens.GREATER_EQUAL, parseLed);
        parser.led(Tokens.LPAREN, parseCallExpression);
        parser.led(Tokens.LBRACKET, parseIndexExpression);

        return parser;
    }

    pub fn repl_init(lexer: *Lexer, allocator: std.mem.Allocator) *Parser {
        const parser = allocator.create(Parser) catch unreachable;

        parser.arena = null;
        parser.allocator = allocator;
        parser.lexer = lexer;

        parser.errors = std.ArrayList(ParserError).init(parser.allocator);

        // Initializing hashmaps
        parser.nud_handlers = std.AutoHashMap(Tokens, NudParseFn).init(parser.allocator);
        parser.led_handlers = std.AutoHashMap(Tokens, LedParseFn).init(parser.allocator);
        parser.binding_powers = std.AutoHashMap(Tokens, Precedence).init(parser.allocator);

        // Setting up the binding power hashtable
        parser.bindingPower(Tokens.EQUAL_EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.NOT_EQUAL, Precedence.EQUALS);
        parser.bindingPower(Tokens.LESST, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.GREATERT, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.LESS_EQUAL, Precedence.LESS_GREATER);
        parser.bindingPower(Tokens.GREATER_EQUAL, Precedence.LESS_GREATER);
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
        parser.nud(Tokens.STRING, parseString);
        parser.nud(Tokens.NOT, parseNud);
        parser.nud(Tokens.MINUS, parseNud);
        parser.nud(Tokens.LPAREN, parseGroupExpression);
        parser.nud(Tokens.IF, parseIfExpression);
        parser.nud(Tokens.FN, parseFnExpression);

        parser.led(Tokens.PLUS, parseLed);
        parser.led(Tokens.MINUS, parseLed);
        parser.led(Tokens.FSLASH, parseLed);
        parser.led(Tokens.ASTERISK, parseLed);
        parser.led(Tokens.EQUAL_EQUAL, parseLed);
        parser.led(Tokens.NOT_EQUAL, parseLed);
        parser.led(Tokens.LESST, parseLed);
        parser.led(Tokens.GREATERT, parseLed);
        parser.led(Tokens.LESS_EQUAL, parseLed);
        parser.led(Tokens.GREATER_EQUAL, parseLed);
        parser.led(Tokens.LPAREN, parseCallExpression);

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        if (self.arena) |arena| {
            for (self.errors.items) |err| {
                self.allocator.free(err.message);
            }

            arena.deinit();
        }
    }

    /// Emits a parser error for the peek token
    pub fn peekError(self: *Parser, token_literal: []const u8) void {
        if (self.in_panic) return;

        self.had_error = true;
        self.in_panic = true;

        const token = self.peek_token;
        const source = dbg.getSourceLine(self.lexer.source, token.position);
        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, token.position, source);
        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);

        const fullerrMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| syntax error: expected {s} but got {s}
            \\
            \\
        , .{ token.position.filename, token.position.line, token.position.column, token.position.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, token_literal, token.literal }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        errh.printError(fullerrMsg, .{});

        self.errors.append(ParserError{ .message = self.allocator.dupe(u8, fullerrMsg) catch |_err| {
            errh.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errh.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    /// Emits a parser error for the current token being processed, but differently from pError that takes in a single message, this one takes in a formatted message
    pub fn eError(self: *Parser, comptime errorMessage: []const u8, varargs: anytype) void {
        if (self.in_panic) {
            return;
        }

        self.had_error = true;
        self.in_panic = true;

        const token = self.cur_token;
        const source = dbg.getSourceLine(self.lexer.source, token.position);
        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, token.position, source);
        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);

        const errMsg = std.fmt.allocPrint(self.allocator, errorMessage, varargs) catch unreachable;

        const fullerrMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| syntax error: {s}
            \\
            \\
        , .{ token.position.filename, token.position.line, token.position.column, token.position.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, errMsg }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        errh.printError(fullerrMsg, .{});

        self.errors.append(ParserError{ .message = self.allocator.dupe(u8, fullerrMsg) catch |_err| {
            errh.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errh.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    /// Emits a parser error with a custom message for the current token being processed
    pub fn pError(self: *Parser, errorMessage: []const u8) void {
        if (self.in_panic) {
            return;
        }

        self.had_error = true;
        self.in_panic = true;

        const token = self.cur_token;

        const source = dbg.getSourceLine(self.lexer.source, token.position);
        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, token.position, source);
        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);

        const errMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| syntax error: {s} but got {s}
            \\
            \\
        , .{ token.position.filename, token.position.line, token.position.column, token.position.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, errorMessage, token.literal }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        errh.printError(errMsg, .{});

        self.errors.append(ParserError{ .message = std.heap.page_allocator.dupe(u8, errMsg) catch |_err| {
            errh.exitWithError("unrecoverable error trying to dupe parse error message", _err);
        } }) catch |err| {
            errh.exitWithError("unrecoverable error trying to append parse error message", err);
        };
    }

    pub fn sync(self: *Parser) noreturn {
        self.in_panic = false;

        while (self.cur_token.token_type != .EOF) {
            switch (self.cur_token.token_type) {
                .VAR, .FN, .RETURN, .IF => return,
                else => self.advance(),
            }
        }
    }

    pub inline fn bindingPower(self: *Parser, token_type: Tokens, prec: Precedence) void {
        self.binding_powers.put(token_type, prec) catch |err| {
            errh.exitWithError("Error registering binding power", err);
        };
    }

    pub inline fn nud(self: *Parser, token_type: Tokens, func: NudParseFn) void {
        self.nud_handlers.put(token_type, func) catch |err| {
            errh.exitWithError("Error registering nud(prefix) parse function", err);
        };
    }

    pub inline fn led(self: *Parser, token_type: Tokens, func: LedParseFn) void {
        self.led_handlers.put(token_type, func) catch |err| {
            errh.exitWithError("Error registering led(infix) parse function", err);
        };
    }

    fn tryPrefixConstantFold(self: *Parser, expr: ?*AST.Expression, token: Token) ?AST.Expression {
        switch (token.token_type) {
            .NOT => {
                switch (expr.?.*) {
                    .boolean_expr => |n| {
                        if (n.value) {
                            return AST.Expression{ .boolean_expr = AST.BooleanExpression{ .token = Token.makeToken(Tokens.FALSE, "FALSE", token.position), .value = false } };
                        }
                        return AST.Expression{ .boolean_expr = AST.BooleanExpression{ .token = Token.makeToken(Tokens.TRUE, "TRUE", token.position), .value = true } };
                    },
                    else => return null,
                }
            },
            .MINUS => {
                switch (expr.?.*) {
                    .number_expr => |n| {
                        const strNum = std.fmt.allocPrint(self.allocator, "{d}", .{-n.value}) catch unreachable;
                        return AST.Expression{ .number_expr = AST.NumberExpression{ .token = Token.makeToken(Tokens.NUMBER, strNum, token.position), .value = -n.value } };
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    pub fn parseNud(self: *Parser) ?*AST.Expression {
        const cur_token = self.cur_token;

        self.advance();

        const rightExpression = self.parseExpression(Precedence.PREFIX);
        const expr = self.createExpressionNode();

        if (self.tryPrefixConstantFold(rightExpression, cur_token)) |_expr| {
            expr.* = _expr;
            return expr;
        }

        const prefixExpr = AST.PrefixExpression{ .token = cur_token, .right = rightExpression };

        expr.* = AST.Expression{ .prefix_expr = prefixExpr };

        return expr;
    }

    fn concatenateStrings(self: *Parser, left: []const u8, right: []const u8) []const u8 {
        const result = self.allocator.alloc(u8, left.len + right.len) catch unreachable;
        @memcpy(result[0..left.len], left);
        @memcpy(result[left.len..], right);
        return result;
    }

    fn tryConstantFold(self: *Parser, left_expr: ?*AST.Expression, right_expr: ?*AST.Expression, token: Token) ?AST.Expression {
        if (left_expr == null or right_expr == null) return null;

        const left = left_expr.?.*;
        const right = right_expr.?.*;

        if (left == .number_expr and right == .number_expr) {
            const left_val = left.number_expr.value;
            const right_val = right.number_expr.value;

            const makeNumberExpr = struct {
                fn call(parser: *Parser, value: f64, pos: Position) AST.Expression {
                    const str_num = std.fmt.allocPrint(parser.allocator, "{d}", .{value}) catch unreachable;
                    return AST.Expression{ .number_expr = AST.NumberExpression{ .token = Token.makeToken(Tokens.NUMBER, str_num, pos), .value = value } };
                }
            }.call;

            const makeBoolExpr = struct {
                fn call(value: bool, pos: Position) AST.Expression {
                    const token_type = if (value) Tokens.TRUE else Tokens.FALSE;
                    const token_str = if (value) "TRUE" else "FALSE";
                    return AST.Expression{ .boolean_expr = AST.BooleanExpression{ .token = Token.makeToken(token_type, token_str, pos), .value = value } };
                }
            }.call;

            return switch (token.token_type) {
                .PLUS => makeNumberExpr(self, left_val + right_val, token.position),
                .MINUS => makeNumberExpr(self, left_val - right_val, token.position),
                .ASTERISK => makeNumberExpr(self, left_val * right_val, token.position),
                .FSLASH => if (right_val == 0) null else makeNumberExpr(self, left_val / right_val, token.position),
                .NOT_EQUAL => makeBoolExpr(left_val != right_val, token.position),
                .EQUAL_EQUAL => makeBoolExpr(left_val == right_val, token.position),
                .GREATERT => makeBoolExpr(left_val > right_val, token.position),
                .LESST => makeBoolExpr(left_val < right_val, token.position),
                .LESS_EQUAL => makeBoolExpr(left_val <= right_val, token.position),
                .GREATER_EQUAL => makeBoolExpr(left_val >= right_val, token.position),
                else => null,
            };
        }

        if (left == .string_expr and right == .string_expr and token.token_type == .PLUS) {
            const left_val = left.string_expr.value;
            const right_val = right.string_expr.value;

            const result = self.concatenateStrings(left_val, right_val);
            return AST.Expression{ .string_expr = AST.StringExpression{ .token = Token.makeToken(Tokens.STRING, result, token.position), .value = result } };
        }

        return null;
    }

    pub fn parseLed(self: *Parser, left_expr: ?*AST.Expression) ?*AST.Expression {
        const cur_token = self.cur_token;
        const prec = self.curBindingPower();

        var infixExpr = AST.InfixExpression{ .token = cur_token, .left = left_expr };

        self.advance();

        const rightExpression = self.parseExpression(prec);
        infixExpr.right = rightExpression;

        const expr = self.createExpressionNode();

        if (self.tryConstantFold(left_expr, rightExpression, cur_token)) |folded_node| {
            expr.* = folded_node;
            return expr;
        }

        expr.* = AST.Expression{ .infix_expr = infixExpr };
        return expr;
    }

    pub fn parseCallArgs(self: *Parser) ?[]?*AST.Expression {
        var args = std.BoundedArray(?*AST.Expression, 32).init(0) catch unreachable;

        if (self.peekIs(Tokens.RPAREN)) {
            self.advance();

            return &[_]?*AST.Expression{};
        }

        self.advance();

        const arg = self.parseExpression(Precedence.DEFAULT) orelse null;

        args.append(arg) catch |err| {
            errh.exitWithError("Unrecoverable error when trying to append function argument.", err);
        };

        while (self.peekIs(Tokens.COMMA)) {
            self.advance(); // advance after the comma
            self.advance(); // advance again, this time cur_token becomes the next argument

            const _arg = self.parseExpression(Precedence.DEFAULT) orelse null;
            args.append(_arg) catch |err| {
                errh.exitWithError("Unrecoverable error when trying to append function argument.", err);
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
        if (args != null) {
            callExpr.arguments.appendSlice(args.?) catch unreachable;
        }

        expr.* = AST.Expression{ .call_expr = callExpr };

        return expr;
    }

    pub fn parseIndexExpression(self: *Parser, left_expr: ?*AST.Expression) ?*AST.Expression {
        const expr = self.createExpressionNode();
        const cur_token = self.cur_token;

        self.advance();
        expr.* = AST.Expression{ .index_expr = AST.IndexExpression{ .token = cur_token, .left = left_expr, .index = self.parseExpression(Precedence.DEFAULT) } };

        if (!self.expect(Tokens.RBRACKET, "]")) {
            return null;
        }

        return expr;
    }

    pub fn parseGroupExpression(self: *Parser) ?*AST.Expression {
        self.advance();

        const expression = self.parseExpression(Precedence.DEFAULT) orelse return null;

        const expr = self.createExpressionNode();
        expr.* = expression.*;

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
                self.pError("expected }");
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

        if (self.peekIs(Tokens.RPAREN)) {
            self.advance();

            return &[_]AST.Identifier{};
        }

        self.advance();

        const ident = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal };
        params.append(ident) catch |err| {
            errh.exitWithError("Unrecoverable error when trying to append function parameter.", err);
        };

        while (self.peekIs(Tokens.COMMA)) {
            self.advance(); // advance after the comma
            self.advance(); // advance again, this time cur_token becomes the next parameter

            const _ident = AST.Identifier{ .token = self.cur_token, .literal = self.cur_token.literal };
            params.append(_ident) catch |err| {
                errh.exitWithError("Unrecoverable error when trying to append function parameter.", err);
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

        if (params != null) {
            fnLiteral.parameters.appendSlice(params.?) catch |err| {
                errh.exitWithError("Unrecoverable error when trying to append slice of function parameters.", err);
            };
        }

        if (!self.expect(Tokens.LBRACE, "{")) {
            return null;
        }

        fnLiteral.body = self.parseBlockStatement() orelse null;

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .fn_expr = fnLiteral };

        return expr;
    }

    pub fn parseExpressionItems(self: *Parser) ?[]?*AST.Expression {
        var items = std.ArrayList(?*AST.Expression).init(self.allocator);

        if (self.peekIs(Tokens.RBRACKET)) {
            self.advance();
            return null;
        }

        self.advance();
        items.append(self.parseExpression(Precedence.DEFAULT)) catch unreachable;

        while (self.peekIs(Tokens.COMMA)) {
            self.advance(); // advance after the comma
            self.advance(); // advance again, this time cur_token becomes the next item

            items.append(self.parseExpression(Precedence.DEFAULT)) catch |err| {
                errh.exitWithError("Unrecoverable error when trying to append function parameter.", err);
            };
        }

        if (!self.expect(Tokens.RBRACKET, "]")) {
            return null;
        }

        return items.toOwnedSlice() catch unreachable;
    }

    pub fn parseArrayExpression(self: *Parser) ?*AST.Expression {
        var arrayLiteral = AST.ArrayExpression.init(self.allocator, self.cur_token);

        if (self.parseExpressionItems()) |list| {
            arrayLiteral.items.appendSlice(list) catch unreachable;
        }

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .array_expr = arrayLiteral };

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

    pub fn parseString(self: *Parser) ?*AST.Expression {
        const str_exp = AST.StringExpression{ .token = self.cur_token, .value = self.cur_token.literal };

        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .string_expr = str_exp };

        return expr;
    }

    pub fn parseBoolean(self: *Parser) ?*AST.Expression {
        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .boolean_expr = AST.BooleanExpression{ .token = self.cur_token, .value = self.currentIs(Tokens.TRUE) } };
        return expr;
    }

    pub fn parseNil(self: *Parser) ?*AST.Expression {
        const expr = self.createExpressionNode();
        expr.* = AST.Expression{ .nil_expr = AST.NilExpression{ .token = self.cur_token, .value = void{} } };
        return expr;
    }

    pub fn createExpressionNode(self: *Parser) *AST.Expression {
        const expr = self.allocator.create(AST.Expression) catch |err| {
            errh.exitWithError("Unrecoverable error when trying to create expression node.", err);
        };

        return expr;
    }

    pub inline fn advance(self: *Parser) void {
        self.previous_token = self.cur_token;
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
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
            return false;
        }
    }

    pub fn expectExpressionToken(self: *Parser, token: Token) bool {
        if (token.token_type == Tokens.ILLEGAL) {
            // Just a hack. If its illegal, we let the parser continue to cascade to a different type of error
            self.advance();
            return false;
        }

        _ = self.nud_handlers.get(token.token_type) orelse {
            self.eError("expected an expression after {s}", .{self.cur_token.literal});
            return true;
        };

        self.advance();
        return false;
    }

    pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) !?*AST.Program {
        const program = AST.Program.init(allocator) orelse return null;

        self.advance();
        self.advance();

        while (self.cur_token.token_type != Tokens.EOF) {
            if (self.parseNode()) |node| {
                try program.addNode(node);
            }

            if (self.in_panic){
                self.sync();
                continue;
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

        if (self.expectExpressionToken(self.peek_token)) {
            return null;
        }

        const expression = self.parseExpression(Precedence.DEFAULT);

        return AST.VarStatement{ .token = var_token, .identifier = identifier, .expression = expression };
    }

    pub fn parseReturnToken(self: *Parser) ?AST.ReturnStatement {
        var rstmt = AST.ReturnStatement{ .token = self.cur_token };
        self.advance();

        const expression = self.parseExpression(Precedence.DEFAULT);

        rstmt.expression = expression;

        return rstmt;
    }

    pub fn parseExpressionStatement(self: *Parser) ?AST.ExpressionStatement {
        var estmt = AST.ExpressionStatement{ .token = self.cur_token };
        estmt.expression = self.parseExpression(Precedence.DEFAULT);

        return estmt;
    }

    pub fn parseExpression(self: *Parser, prec: Precedence) ?*AST.Expression {
        const prefix_fn = self.nud_handlers.get(self.cur_token.token_type) orelse {
            self.pError("expected an expression");
            return null;
        };

        // call the prefix function to parse the left-hand side
        var left = prefix_fn(@constCast(self));

        // infix stuff
        while (@intFromEnum(prec) < @intFromEnum(self.peekBindingPower())) {
            const infix_fn = self.led_handlers.get(self.peek_token.token_type) orelse return left;

            self.advance();

            // call the infix function to parse the right-hand side
            left = infix_fn(@constCast(self), left);
        }

        return left;
    }

    pub fn parseNode(self: *Parser) ?AST.Statement {
        switch (self.cur_token.token_type) {
            Tokens.VAR => {
                if (self.parseVarToken()) |stmt| {
                    return AST.Statement{ .var_stmt = stmt };
                } else {
                    return null;
                }
            },
            Tokens.RETURN => {
                if (self.parseReturnToken()) |stmt| {
                    return AST.Statement{ .return_stmt = stmt };
                } else {
                    return null;
                }
            },
            else => {
                if (self.parseExpressionStatement()) |stmt| {
                    return AST.Statement{ .expression_stmt = stmt };
                } else {
                    return null;
                }
            },
        }

        return null;
    }
};

test "Parser initializtion" {
    const input: []const u8 =
        \\var name = "Fábio Gabriel Rodrigues Varela"
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
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
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
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
        dbg.printVarStatement(node.var_stmt);
    }
}

test "Var statement parsing errors len" {
    const input: []const u8 =
        \\var b 15
        \\var a 55
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
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
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
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
        dbg.printPrefixExpression(node.expression_stmt.expression.?.*.prefix_expr);
    }
}

test "simple if parsing" {
    const input: []const u8 =
        \\if(true){
        \\ var age = 22
        \\}
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
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
    //    dbg.printIfExpression(node.expression_stmt.expression.?.*.if_expr);
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
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        dbg.printIfExpression(node.expression_stmt.expression.?.*.if_expr);
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
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        dbg.printFnExpression(node.expression_stmt.expression.?.*.fn_expr);
    }
}

test "function call" {
    const input: []const u8 =
        \\add()
        \\add(1, 2)
        \\add(1, 2, 3)
    ;

    var l: Lexer = try Lexer.init(input);
    var p: Parser = Parser.init(&l, std.heap.page_allocator);
    defer p.deinit();

    const program = try p.parseProgram(std.heap.page_allocator);

    if (program == null) {
        std.debug.print("Error parsing program: program is null\n", .{});
        try expect(false);
    }

    defer program.?.deinit();

    try expect(p.errors.items.len == 0);

    for (program.?.nodes.items) |node| {
        dbg.printFnExpressionCall(node.expression_stmt.expression.?.*.call_expr);
    }
}

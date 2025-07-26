const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const mem = std.mem;
const ascii = std.ascii;
const unicode = std.unicode;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Keywords = _token.Keywords;
const KeywordMap = _token.KeywordMap;
const Position = _token.Position;
const dbg = @import("debug.zig");

pub const Lexer = struct {
    arena: std.heap.ArenaAllocator,

    source: []const u8,
    filename: []const u8,
    iterator: unicode.Utf8Iterator,
    currentChar: ?[]const u8,

    column: usize,
    line: usize,
    current: u32,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, filename: []const u8) *Lexer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const lexer = arena.allocator().create(Lexer) catch unreachable;

        var iterator = unicode.Utf8View.init(source) catch unreachable;

        lexer.arena = arena;
        lexer.filename = filename;
        lexer.source = source;
        lexer.iterator = iterator.iterator();
        lexer.column = 1;
        lexer.line = 1;
        lexer.current = 0;
        lexer.currentChar = null;

        return lexer;
    }

    pub fn deinit(self: *Lexer) void {
        self.arena.deinit();
    }

    pub fn advance(self: *Lexer) ?[]const u8 {
        if (self.iterator.nextCodepointSlice()) |char| {
            self.currentChar = char;

            self.current += @intCast(char.len);

            if (char.len == 1 and char[0] == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            return char;
        } else {
            self.currentChar = null;
            return null;
        }
    }

    pub fn peek(self: *Lexer, n: usize) []const u8 {
        return self.iterator.peek(n);
    }

    pub fn peekByte(self: *Lexer, n: usize) u8 {
        const slice = self.iterator.peek(n);
        return if (slice.len == 1) slice[0] else 0;
    }


    pub fn eatWhitespace(self: *Lexer) void {
        while (true) {
            const codep = self.peekByte(1);

            switch (codep) {
                ' ', '\t', '\r', ascii.control_code.vt, ascii.control_code.ff => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance().?;
                },
                '/' => {
                    if (self.peekByte(2) == '/') {
                        // Skip single-line comments
                        while (self.peekByte(1) != '\n' and self.peekByte(1) != 0) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn isAsciiAlpha(self: *Lexer, ch: []const u8) bool {
        _ = self;
        if (ch.len != 1) return false;
        const c = ch[0];
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
    }

    fn isAsciiDigit(self: *Lexer, ch: []const u8) bool {
        _ = self;
        if (ch.len != 1) return false;
        const c = ch[0];
        return c >= '0' and c <= '9';
    }

    fn isIdentifier(self: *Lexer, char: []const u8) bool {
        _ = self;
        if (char.len == 0) return false;
        if (char.len == 1) {
            const c = char[0];
            return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
        }

        // For non-ASCII, allow any UTF-8
        return true;
    }

    pub fn identifyTypeOfAlphanumeric(self: *Lexer, identifier: []u8, startColumn: usize, startLine: usize) Token {
        const pos = Position{ .column = startColumn, .line = startLine, .filename = self.filename };

        if (KeywordMap.get(identifier)) |kw| {
            return switch (kw) {
                .FN => Token.makeToken(Tokens.FN, "FN", pos),
                .VAR => Token.makeToken(Tokens.VAR, "VAR", pos),
                .IF => Token.makeToken(Tokens.IF, "IF", pos),
                .ELSE => Token.makeToken(Tokens.ELSE, "ELSE", pos),
                .FOR => Token.makeToken(Tokens.FOR, "FOR", pos),
                .THEN => Token.makeToken(Tokens.THEN, "THEN", pos),
                .END => Token.makeToken(Tokens.END, "END", pos),
                .NIL => Token.makeToken(Tokens.NIL, "NIL", pos),
                .TRUE => Token.makeToken(Tokens.TRUE, "TRUE", pos),
                .FALSE => Token.makeToken(Tokens.FALSE, "FALSE", pos),
                else => unreachable,
            };
        } else {
            return Token.makeToken(Tokens.IDENT, identifier, pos);
        }
    }

    pub fn lexNumber(self: *Lexer, startColumn: usize, startLine: usize) Token {
        var numbers = std.ArrayList(u8).init(self.arena.allocator());
        defer numbers.deinit();

        numbers.appendSlice(self.currentChar.?) catch unreachable;

        while (self.isAsciiDigit(self.peek(1))) {
            numbers.appendSlice(self.advance().?) catch unreachable;
        }

        if (self.peekByte(1) == '.') {
            numbers.appendSlice(self.advance().?) catch unreachable; // Consume the .

            if (!self.isAsciiDigit(self.peek(1))) {
                const pos = Position{ .column = self.column, .line = startLine, .filename = self.filename };
                return Token.makeIllegalToken("malformed number", pos);
            }

            while (self.isAsciiDigit(self.peek(1))) {
                numbers.appendSlice(self.advance().?) catch unreachable;
            }
        }

        const pos = Position{ .column = startColumn, .line = startLine, .filename = self.filename };
        const fullNumber = numbers.toOwnedSlice() catch unreachable;

        return Token.makeToken(Tokens.NUMBER, fullNumber, pos);
    }

    pub fn lexString(self: *Lexer, startColumn: usize, startLine: usize) Token {
        var chars = std.ArrayList(u8).init(self.arena.allocator());
        defer chars.deinit();

        while (self.peekByte(1) != '"') {
            if (self.peekByte(1) == '\n' or self.currentChar.?[0] == '\n') {
                self.line = self.line + 1;
            }
            if (self.advance()) |c| {
                chars.appendSlice(c) catch unreachable;
            } else {
                const pos = Position{ .column = startColumn + 1, .line = startLine, .filename = self.filename };
                return Token.makeIllegalToken("unterminated string", pos);
            }
        }

        _ = self.advance(); // ending "

        const str = chars.toOwnedSlice() catch unreachable;
        const pos = Position{ .column = startColumn, .line = self.line, .filename = self.filename };

        return Token.makeToken(Tokens.STRING, str, pos);
    }

    pub fn lexIdentifier(self: *Lexer, startColumn: usize, startLine: usize) Token {
        var iden = std.ArrayList(u8).init(self.arena.allocator());
        defer iden.deinit();

        iden.appendSlice(self.currentChar.?) catch unreachable;

        while (self.isIdentifier(self.peek(1))) {
            iden.appendSlice(self.advance().?) catch unreachable;
        }

        const identifier = iden.toOwnedSlice() catch unreachable;
        return self.identifyTypeOfAlphanumeric(identifier, startColumn, startLine);
    }

    pub fn nextToken(self: *Lexer) Token {
        self.eatWhitespace();

        const startColumn = self.column;
        const startLine = self.line;

        const ch: ?[]const u8 = self.advance();

        const pos = Position{ .column = startColumn, .line = startLine, .filename = self.filename };

        if (ch == null) {
            return Token.makeToken(Tokens.EOF, "EOF", pos);
        }

        if (self.isIdentifier(ch.?)) {
            return self.lexIdentifier(startColumn, startLine);
        }

        if (self.isAsciiDigit(ch.?)) {
            return self.lexNumber(startColumn, startLine);
        }

        const firstChar: u8 = ch.?[0];
        switch (firstChar) {
            '|' => {
                return Token.makeToken(Tokens.PIPE, "|", pos);
            },
            '%' => {
                return Token.makeToken(Tokens.MODULO, "%", pos);
            },
            '&' => {
                return Token.makeToken(Tokens.BIT_AND, "&", pos);
            },
            ',' => {
                return Token.makeToken(Tokens.COMMA, ",", pos);
            },
            '.' => {
                return Token.makeToken(Tokens.DOT, ".", pos);
            },
            '/' => {
                return Token.makeToken(Tokens.FSLASH, "/", pos);
            },
            '+' => {
                return Token.makeToken(Tokens.PLUS, "+", pos);
            },
            '-' => {
                return Token.makeToken(Tokens.MINUS, "-", pos);
            },
            '*' => {
                return Token.makeToken(Tokens.ASTERISK, "*", pos);
            },
            '[' => {
                return Token.makeToken(Tokens.LBRACKET, "[", pos);
            },
            ']' => {
                return Token.makeToken(Tokens.RBRACKET, "]", pos);
            },
            '(' => {
                return Token.makeToken(Tokens.LPAREN, "(", pos);
            },
            ')' => {
                return Token.makeToken(Tokens.RPAREN, ")", pos);
            },
            '{' => {
                return Token.makeToken(Tokens.LBRACE, "{", pos);
            },
            '}' => {
                return Token.makeToken(Tokens.RBRACE, "}", pos);
            },
            '!' => {
                if (self.peekByte(1) == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.NOT_EQUAL, "!=", pos);
                }
                return Token.makeToken(Tokens.NOT, "!", pos);
            },
            '=' => {
                if (self.peekByte(1) == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.EQUAL_EQUAL, "==", pos);
                }
                return Token.makeToken(Tokens.EQUAL, "=", pos);
            },
            '<' => {
                if (self.peekByte(1) == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.LESS_EQUAL, "<=", pos);
                }

                if (self.peekByte(1) == '<') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.LEFT_SHIFT, "<<", pos);
                }
                return Token.makeToken(Tokens.LESST, "<", pos);
            },
            '>' => {
                if (self.peekByte(1) == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.GREATER_EQUAL, ">=", pos);
                }

                if (self.peekByte(1) == '>') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.RIGHT_SHIFT, ">>", pos);
                }
                return Token.makeToken(Tokens.GREATERT, ">", pos);
            },
            '"' => {
                return self.lexString(startColumn, startLine);
            },
            else => {
                return Token.makeIllegalToken("unexpected character", pos);
            },
        }
    }
};

test "Lexer initialization" {
    const input: []const u8 = "test input";
    const l: Lexer = try Lexer.init(input);

    try expect(mem.eql(u8, input, l.source));
}

test "Input tokenization" {
    const source =
        \\ / + - *
        \\10.10
        \\11.50
        \\1
        \\2
        \\"a string"
        \\
    ;

    var l: Lexer = try Lexer.init(source);
    const testArr: [9]Token = .{
        Token{ .token_type = Tokens.FSLASH, .literal = "/" },
        Token{ .token_type = Tokens.PLUS, .literal = "+" },
        Token{ .token_type = Tokens.MINUS, .literal = "-" },
        Token{ .token_type = Tokens.ASTERISK, .literal = "*" },
        Token{ .token_type = Tokens.NUMBER, .literal = "10.10" },
        Token{ .token_type = Tokens.NUMBER, .literal = "11.50" },
        Token{ .token_type = Tokens.NUMBER, .literal = "1" },
        Token{ .token_type = Tokens.NUMBER, .literal = "2" },
        Token{ .token_type = Tokens.STRING, .literal = "a string" },
    };

    var idx: usize = 0;
    while (true) : (idx += 1) {
        const actual = l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break; // Exit the loop on EOF
        }
        dbg.printToken(actual);
        const expected = testArr[idx];
        if (expected.token_type != actual.token_type) {
            std.debug.print("Token mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
        if (!std.mem.eql(u8, expected.literal, actual.literal)) {
            std.debug.print("Token literal mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
    }
}

test "Variable declaration tokenization" {
    const source =
        \\var a = 5
        \\var b = 10
        \\var c = 15
        \\var d = 20.20
        \\var e = "25"
        \\var f = "3" + "0"
        \\var g = nil
    ;

    var l: Lexer = try Lexer.init(source);
    const testArr: [30]Token = .{
        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "a" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "5" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "b" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "10" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "c" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "15" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "d" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "20.20" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "e" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.STRING, .literal = "25" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "f" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.STRING, .literal = "3" },
        Token{ .token_type = Tokens.PLUS, .literal = "+" },
        Token{ .token_type = Tokens.STRING, .literal = "0" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "g" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NIL, .literal = "NIL" },
    };

    var idx: usize = 0;
    while (true) : (idx += 1) {
        const actual = l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break;
        }

        //dbg.printToken(actual);
        const expected = testArr[idx];
        if (expected.token_type != actual.token_type) {
            std.debug.print("Token mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
        if (!std.mem.eql(u8, expected.literal, actual.literal)) {
            std.debug.print("Token literal mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
    }
}

test "Simple utf8 lexing" {
    const source =
        \\var ço = 5
        \\var á = "até"
        \\var _name = "Fábio Gabriel Rodrigues Varela"
    ;

    var l: Lexer = try Lexer.init(source);
    const testArr: [16]Token = .{
        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "ço" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "5" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "á" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.STRING, .literal = "até" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "_name" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.STRING, .literal = "Fábio Gabriel Rodrigues Varela" },

        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "d" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.NUMBER, .literal = "20.20" },
    };

    var idx: usize = 0;
    while (true) : (idx += 1) {
        const actual = l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break;
        }

        const expected = testArr[idx];
        if (expected.token_type != actual.token_type) {
            std.debug.print("Token mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
        if (!std.mem.eql(u8, expected.literal, actual.literal)) {
            std.debug.print("Token literal mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
    }
}

test "Another Simple utf8 lexing" {
    const source =
        \\var 名前 = "私の名前はファビオ・バレラです。"
    ;

    var l: Lexer = try Lexer.init(source);
    const testArr: [4]Token = .{
        Token{ .token_type = Tokens.VAR, .literal = "VAR" },
        Token{ .token_type = Tokens.IDENT, .literal = "名前" },
        Token{ .token_type = Tokens.EQUAL, .literal = "=" },
        Token{ .token_type = Tokens.STRING, .literal = "私の名前はファビオ・バレラです。" },
    };

    var idx: usize = 0;
    while (true) : (idx += 1) {
        const actual = l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break;
        }

        const expected = testArr[idx];
        if (expected.token_type != actual.token_type) {
            std.debug.print("Token mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
        if (!std.mem.eql(u8, expected.literal, actual.literal)) {
            std.debug.print("Token literal mismatch: expected {s}, got {s}\n", .{ expected.literal, actual.literal });
            try expect(false);
        }
    }
}

const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const mem = std.mem;
const ascii = std.ascii;
const unicode = std.unicode;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;
const debug = @import("debug.zig");

inline fn isSpace(char: u8) bool {
    return mem.indexOfScalar(u8, &ascii.whitespace, char) != null;
}

pub const Lexer = struct {
    content: []const u8,
    iterator: unicode.Utf8Iterator,
    currentChar: ?u8 = null,
    column: usize = 0,
    line: usize = 1,

    pub fn init(content: []const u8) !Lexer {
        var iterator = (try unicode.Utf8View.init(content));
        const lexer = Lexer{ .content = content, .iterator = iterator.iterator() };
        return lexer;
    }

    pub fn advance(self: *Lexer) ?u8 {
        if (self.iterator.nextCodepointSlice()) |char| {
            self.currentChar = char[0];
            self.column = self.column + 1;
            return char[0];
        } else {
            self.currentChar = null;
            return null;
        }
    }

    pub fn peekNBytes(self: *Lexer, bytes: usize) u8 {
        const peeked = self.iterator.peek(bytes);
        if (peeked.len == 0) {
            return 0;
        }
        return peeked[0];
    }

    pub fn peek(self: *Lexer) u8 {
        const peeked = self.iterator.peek(1);
        if (peeked.len == 0) {
            return 0;
        }
        return peeked[0];
    }

    pub fn eatWhitespace(self: *Lexer) void {
        while (isSpace(self.peek())) {
            _ = self.advance();

            switch (self.currentChar.?) {
                ' ', '\t', '\r' => {},

                '\n' => {
                    self.line = self.line + 1;
                },
                else => {},
            }
        }
    }

    // trie
    pub fn identifyTypeOfAlphanumeric(self: *Lexer, identifier: []u8) Token {
        const pos = Position{ .column = self.column, .line = self.line };

        switch (identifier[0]) {
            'p' => {
                if (mem.eql(u8, identifier, "print")) {
                    return Token.makeToken(Tokens.PRINT, "PRINT", pos);
                }
            },
            'v' => {
                if (mem.eql(u8, identifier, "var")) {
                    return Token.makeToken(Tokens.VAR, "VAR", pos);
                }
            },
            'e' => {
                if (mem.eql(u8, identifier, "else")) {
                    return Token.makeToken(Tokens.ELSE, "ELSE", pos);
                }
            },
            'i' => {
                if (mem.eql(u8, identifier, "if")) {
                    return Token.makeToken(Tokens.IF, "IF", pos);
                }
            },
            'n' => {
                if (mem.eql(u8, identifier, "nil")) {
                    return Token.makeToken(Tokens.NIL, "NIL", pos);
                }
            },
            'r' => {
                if (mem.eql(u8, identifier, "return")) {
                    return Token.makeToken(Tokens.RETURN, "RETURN", pos);
                }
            },
            't' => {
                if (mem.eql(u8, identifier, "true")) {
                    return Token.makeToken(Tokens.TRUE, "TRUE", pos);
                }
            },
            'f' => {
                if (mem.eql(u8, identifier, "false")) {
                    return Token.makeToken(Tokens.FALSE, "FALSE", pos);

                }

                if (mem.eql(u8, identifier, "fn")) {
                    return Token.makeToken(Tokens.FN, "FN", pos);

                }
            },
            else => {
                return Token.makeToken(Tokens.IDENT, identifier, pos);
            },
        }

        return Token.makeToken(Tokens.IDENT, identifier, pos);
    }

    pub fn lexNumber(self: *Lexer) !Token {
        var numbers = std.ArrayList(u8).init(std.heap.page_allocator);
        defer numbers.deinit();

        try numbers.append(self.currentChar.?);

        while (ascii.isDigit(self.peek())) {
            try numbers.append(self.advance().?);
        }

        if (self.peek() == '.') {
            try numbers.append(self.advance().?); // Consume the .

            if (!ascii.isDigit(self.peek())) {}

            while (ascii.isDigit(self.peek())) {
                try numbers.append(self.advance().?);
            }
        }

        const pos = Position{ .column = self.column, .line = self.line };
        const fullNumber = try numbers.toOwnedSlice();
        return Token.makeToken(Tokens.NUMBER, fullNumber, pos);
    }

    pub fn lexString(self: *Lexer) !Token {
        var chars = std.ArrayList(u8).init(std.heap.page_allocator);
        defer chars.deinit();

        while (self.peek() != '"') {
            if (self.peek() == '\n' or self.currentChar.? == '\n') {
                self.line = self.line + 1;
            }
            try chars.append(self.advance().?);
        }

        const pos = Position{ .column = self.column, .line = self.line };
        if (self.currentChar == null) {
            return Token.makeIllegalToken("Unterminated string", pos);
        }

        _ = self.advance(); // ending "
        const str = try chars.toOwnedSlice();
        return Token.makeToken(Tokens.STRING, str, pos);
    }

    pub fn lexAlphanumeric(self: *Lexer) !Token {
        var alphaNum = std.ArrayList(u8).init(std.heap.page_allocator);
        defer alphaNum.deinit();
        try alphaNum.append(self.currentChar.?);

        while (ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
            try alphaNum.append(self.advance().?);
        }

        const identifier = try alphaNum.toOwnedSlice();
        return self.identifyTypeOfAlphanumeric(identifier);
    }

    pub fn nextToken(self: *Lexer) !Token {
        self.eatWhitespace();

        const ch: ?u8 = self.advance();
        const pos = Position{ .column = self.column, .line = self.line };

        if (ch == null) {
            return Token.makeToken(Tokens.EOF, "EOF", pos);
        }

        switch (ch.?) {
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
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.NOT_EQUAL, "!=", pos);
                }
                return Token.makeToken(Tokens.NOT, "!", pos);
            },
            '=' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.EQUAL_EQUAL, "==", pos);
                }
                return Token.makeToken(Tokens.EQUAL, "=", pos);
            },
            '<' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.EQUAL_EQUAL, "<=", pos);
                }
                return Token.makeToken(Tokens.LESST, "<", pos);
            },
            '>' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token.makeToken(Tokens.GREATERT, ">=", pos);
                }
                return Token.makeToken(Tokens.GREATERT, ">", pos);
            },
            '"' => {
                return try self.lexString();
            },
            else => {
                if (ascii.isDigit(ch.?)) {
                    return try self.lexNumber();
                } else if (ascii.isAlphanumeric(ch.?)) {
                    return try self.lexAlphanumeric();
                } else {
                    return Token.makeIllegalToken("ILLEGAL", pos);
                }
            },
        }
    }
};

test "Lexer initialization" {
    const input: []const u8 = "test input";
    const l: Lexer = try Lexer.init(input);

    try expect(mem.eql(u8, input, l.content));
}

test "Input tokenization" {
    const source =
        \\ / + - *
        \\10.10
        \\11.50
        \\1
        \\2
        \\print 10
        \\ "a string"
        \\
    ;

    var l: Lexer = try Lexer.init(source);
    const testArr: [11]Token = .{
        Token{ .token_type = Tokens.FSLASH, .literal = "/" },
        Token{ .token_type = Tokens.PLUS, .literal = "+" },
        Token{ .token_type = Tokens.MINUS, .literal = "-" },
        Token{ .token_type = Tokens.ASTERISK, .literal = "*" },
        Token{ .token_type = Tokens.NUMBER, .literal = "10.10" },
        Token{ .token_type = Tokens.NUMBER, .literal = "11.50" },
        Token{ .token_type = Tokens.NUMBER, .literal = "1" },
        Token{ .token_type = Tokens.NUMBER, .literal = "2" },
        Token{ .token_type = Tokens.PRINT, .literal = "PRINT" },
        Token{ .token_type = Tokens.NUMBER, .literal = "10" },
        Token{ .token_type = Tokens.STRING, .literal = "a string" },
    };

    var idx: usize = 0;
    while (true) : (idx += 1) {
        const actual = try l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break; // Exit the loop on EOF
        }
        debug.printToken(actual);
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
        const actual = try l.nextToken();

        if (actual.token_type == Tokens.EOF) {
            break; // Exit the loop on EOF
        }
        debug.printToken(actual);
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

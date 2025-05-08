const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const mem = std.mem;
const ascii = std.ascii;
const unicode = std.unicode;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

inline fn isSpace(char: u8) bool {
    return mem.indexOfScalar(u8, &ascii.whitespace, char) != null;
}

pub const Lexer = struct {
    content: []const u8,
    iterator: unicode.Utf8Iterator,
    currentChar: ?u8 = null,
    currentSlice: ?[]const u8 = null,
    column: usize = 0,
    line: usize = 0,

    pub fn init(content: []const u8) !Lexer {
        var iterator = (try unicode.Utf8View.init(content));
        const lexer = Lexer{ .content = content, .iterator = iterator.iterator() };
        return lexer;
    }

    pub fn deinit() void {}

    pub inline fn advance(self: *Lexer) ?u8 {
        if (self.iterator.nextCodepointSlice()) |char| {
            self.currentChar = char[0];
            self.currentSlice = char;
            return char[0];
        } else {
            self.currentChar = null;
            self.currentSlice = null;
            return null;
        }
    }

    pub inline fn peekNBytes(self: *Lexer, bytes: usize) []const u8 {
        return self.iterator.peek(bytes);
    }

    pub inline fn peek(self: *Lexer) u8 {
        return self.iterator.peek(1)[0];
    }

    pub fn ignoreWhitespace(self: *Lexer) void {
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

    pub fn lexNumber(self: *Lexer, char: u8) !Token {
        var fullNumber = std.ArrayList(u8).init(std.heap.page_allocator);
        defer fullNumber.deinit();

        try fullNumber.append(char);

        while (ascii.isDigit(self.peek())) {
            try fullNumber.append(self.advance().?);
        }

        if (self.peek() == '.' and ascii.isDigit(self.peek())) {
            try fullNumber.append(self.advance().?); // Consume the .

            while (ascii.isDigit(self.peek())) {
                try fullNumber.append(self.advance().?);
            }
        }

        return Token.makeToken(Tokens.NUMBER, fullNumber.items);
    }

    pub fn lexString(self: *Lexer) !Token {
        var str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer str.deinit();

        while (self.peek() != '"') {
            if (self.peek() == '\n') {
                // increment line here
            }
            try str.append(self.advance().?);
        }

        if (self.currentChar == null) {
            return Token.makeErrorToken("Unterminated string");
        }

        _ = self.advance(); // ending "
        return Token.makeToken(Tokens.STRING, str.items);
    }

    pub fn lexAlphanumeric(self: *Lexer, char: u8) !Token {
        var identifier = std.ArrayList(u8).init(std.heap.page_allocator);
        defer identifier.deinit();
        try identifier.append(char);

        while (ascii.isAlphanumeric(self.peek())) {
            try identifier.append(self.advance().?);
        }
    }

    pub fn nextToken(self: *Lexer) !Token {
        //self.ignoreWhitespace();

        const ch: ?u8 = self.advance();

        if (ch == null) {
            return Token.makeToken(Tokens.EOF, "EOF");
        }

        switch (ch.?) {
            '/' => {
                return Token.makeToken(Tokens.FSLASH, "/");
            },
            '+' => {
                return Token.makeToken(Tokens.PLUS, "+");
            },
            '-' => {
                return Token.makeToken(Tokens.MINUS, "-");
            },
            '*' => {
                return Token.makeToken(Tokens.ASTERISK, "*");
            },
            '(' => {
                return Token.makeToken(Tokens.LPAREN, "(");
            },
            ')' => {
                return Token.makeToken(Tokens.RPAREN, ")");
            },
            '{' => {
                return Token.makeToken(Tokens.LBRACE, "{");
            },
            '}' => {
                return Token.makeToken(Tokens.RBRACE, "}");
            },
            '!' => {
                return Token.makeToken(Tokens.NOT, "!");
            },
            '=' => {
                return Token.makeToken(Tokens.EQUAL, "=");
            },
            '<' => {
                return Token.makeToken(Tokens.LESST, "<");
            },
            '>' => {
                return Token.makeToken(Tokens.GREATERT, ">");
            },
            '"' => {
                return try self.lexString();
            },
            else => {
                // TODO: FINISH THIS (USE ELSE IF)
                //if (ascii.isAlphanumeric(ch.?)) {
                //    return self.lexAlphanumeric(ch.?);
                //}

                if (ascii.isDigit(ch.?)) {
                    return try self.lexNumber(ch.?);
                } else if (isSpace(ch.?)) {} else {
                    return Token.makeErrorToken("ILLEGAL"); // Maybe add a better illegal msg
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

test "Simple input tokenization" {
    const source =
        \\/
        \\+
        \\-
        \\*
    ;
    var l: Lexer = try Lexer.init(source);
    // const testArr: [6]Token = .{
    //     Token{ .token_type = Tokens.NUMBER, .literal = "1" },
    //     Token{ .token_type = Tokens.NUMBER, .literal = "2" },
    //     Token{ .token_type = Tokens.NUMBER, .literal = "3" },
    //     Token{ .token_type = Tokens.NUMBER, .literal = "4" },
    //     Token{ .token_type = Tokens.QUOTE, .literal = "a string" },
    //     Token{ .token_type = Tokens.FSLASH, .literal = "/" },
    //     Token{ .token_type = Tokens.PLUS, .literal = "+" },
    //     Token{ .token_type = Tokens.MINUS, .literal = "-" },
    //    Token{ .token_type = Tokens.ASTERISK, .literal = "*" },
    // };

    while (l.nextToken()) |token| {
        if (token.token_type == Tokens.EOF) {
            break; // Exit the loop on EOF
        }
        print("{s}\n", .{token.literal});
    } else |err| {
        print("{any}\n", .{err});
    }

    try expect(1 == 1); // placeholder
}

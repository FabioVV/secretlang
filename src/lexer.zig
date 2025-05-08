const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const mem = std.mem;
const ascii = std.ascii;
const unicode = std.unicode;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

pub const Lexer = struct {
    content: []const u8,
    iterator: unicode.Utf8Iterator,

    pub fn init(content: []const u8) !Lexer {
        var iterator = (try unicode.Utf8View.init(content));
        const lexer = Lexer{ .content = content, .iterator = iterator.iterator() };
        return lexer;
    }

    pub fn deinit() void {}

    inline fn removeWhitespace(self: *Lexer) void {
        while (self.isSpace(self.iterator.nextCodepointSlice())) {}
    }

    inline fn isSpace(char: u8) bool {
        return mem.indexOfScalar(u8, &ascii.whitespace, char) != null;
    }

    pub inline fn advance(self: *Lexer) ?[]const u8 {
        return self.iterator.nextCodepointSlice();
    }

    pub inline fn peekNBytes(self: *Lexer, bytes: usize) []const u8 {
        return self.iterator.peek(bytes);
    }

    pub fn generateToken(self: *Lexer) !void {
        var token: Token = undefined;
        try self.removeWhitespace();

        const ch: u8 = self.advance();
        switch (ch) {
            '/' => {},
            '+' => {},
            '-' => {},
            '*' => {},
            '(' => {},
            '{' => {},
            '}' => {},
            '!' => {},
            '=' => {},
            '<' => {},
            '>' => {},
            '"' => {},
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
        \\1234
        \\"a string"
        \\/
        \\+
        \\-
        \\*
    ;
    var l: Lexer = try Lexer.init(source);

    while (l.advance()) |char| {
        //var token: Token = undefined;
        print("char: {s}\n", .{char});
    }

    // for (tokens) |t| {
    //     print("Printing generated tokens...\n", .{});
    //     print("{s}\n", .{@tagName(t.token_type)});
    //}

    try expect(1 == 1); // placeholder
}

const std = @import("std");

pub const Lexer = struct {
    content: []const u8,

    pub fn init(content: []const u8) Lexer {
        const lexer = Lexer{ .content = content };
        return lexer;
    }
};

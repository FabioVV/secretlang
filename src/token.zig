pub const Token = struct {
    token_type: Tokens,
    literal: []const u8,

    pub fn makeToken(token_t: Tokens, literal: []const u8) !Token {
        return Token{ .token_type = token_t, .literal = literal };
    }
};

pub const Tokens = enum {
    IDENT, // variables or const names
    NUMBER, // 1234
    STRING, // "this is a string"
    PLUS, // +
    MINUS, // -
    ASTERISK, // *
    FSLASH, // /
};

pub const Token = struct {
    token_type: Tokens,
    literal: []const u8,
    column: usize = 0,
    line: usize = 0,

    pub fn makeToken(token_t: Tokens, literal: []const u8) Token {
        return Token{ .token_type = token_t, .literal = literal };
    }

    pub fn makeErrorToken(message: []const u8) Token {
        return Token{ .token_type = Tokens.ILLEGAL, .literal = message };
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

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    NOT, // !
    EQUAL, // =
    EQUAL_EQUAL, // ==
    LESST, // <
    GREATERT, // >
    QUOTE, // "

    ILLEGAL, // Illegal token
    EOF, // end of file
};

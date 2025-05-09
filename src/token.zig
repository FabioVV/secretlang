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
    DOT, // .

    NOT, // !
    EQUAL, // =
    EQUAL_EQUAL, // ==
    LESST, // <
    GREATERT, // >
    QUOTE, // "

    PRINT, // Print token, it will work as a function, accepts a single expression like: print 1 + 1 or print 1 or print "str" etc..

    VAR, // variable declartion token

    NIL, // Null value

    ILLEGAL, // Illegal token
    EOF, // end of file
};

pub const Token = struct {
    token_type: Tokens,
    literal: []const u8,
    column: usize = 0,
    line: usize = 0,

    pub fn makeToken(token_t: Tokens, literal: []const u8) Token {
        return Token{ .token_type = token_t, .literal = literal };
    }

    pub fn makeIllegalToken(message: []const u8) Token {
        return Token{ .token_type = Tokens.ILLEGAL, .literal = message };
    }
};

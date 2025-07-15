pub const Tokens = enum {
    IDENT, // variables or const names

    NUMBER, // 1234
    STRING, // "this is a string"

    COMMA, // ,

    PLUS, // +
    MINUS, // -
    ASTERISK, // *
    FSLASH, // /

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    LBRACKET, // [
    RBRACKET, // ]

    DOT, // .

    NOT_EQUAL, // !=
    NOT, // !
    EQUAL, // =
    EQUAL_EQUAL, // ==
    LESST, // <
    GREATERT, // >
    QUOTE, // "

    PRINT, // Print token, it will work as a function, accepts a single expression like: print 1 + 1 or print 1 or print "str" etc..

    VAR, // variable declartion token

    IF, // if
    ELSE, // else

    FN, // Function

    RETURN, // return token

    NIL, // Null value
    TRUE, // True bool value
    FALSE, // False bool value

    ILLEGAL, // Illegal token
    EOF, // end of file
};

pub const Position = struct {
    column: usize = 0,
    line: usize = 0,
    filename: []const u8 = "stdin",
};

pub const Token = struct {
    token_type: Tokens,
    literal: []const u8,
    position: Position = undefined, // Just to make our life easir during testing, this way we dont need to pass in a position every time we create a token by hand

    pub fn makeToken(token_t: Tokens, literal: []const u8, position: Position) Token {
        return Token{ .token_type = token_t, .literal = literal, .position = position };
    }

    pub fn makeIllegalToken(message: []const u8, position: Position) Token {
        return Token{ .token_type = Tokens.ILLEGAL, .literal = message, .position = position };
    }
};

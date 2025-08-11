const std = @import("std");

pub const Tokens = enum {
    IDENT, // variables or const names

    NUMBER, // 1234, 1.23, 12.232 // TODO: remove this and use the other two below
    INTEGER,
    FLOAT,
    STRING, // "this is a string"

    COMMA, // ,
    SEMICOLON, // ;

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
    GREATER_EQUAL, //>=
    LESS_EQUAL, //<=
    QUOTE, // "

    LEFT_SHIFT, //<<
    RIGHT_SHIFT, //>>
    BIT_AND, // &

    MODULO, // %
    PIPE, // |

    THEN, //then keyword
    END, // end keyword

    VAR, // variable declartion token

    IF, // if
    ELSE, // else
    FOR, // for loop
    WHILE, // while loop

    FN, // Function

    RETURN, // return token

    NIL, // Null value
    TRUE, // True bool value
    FALSE, // False bool value

    ILLEGAL, // Illegal token
    EOF, // end of file
};

pub const Keywords = enum { FN, IF, ELSE, THEN, END, FOR, WHILE, VAR, NIL, TRUE, FALSE, RETURN };

pub const KeywordMap = std.StaticStringMap(Keywords).initComptime(.{
    .{ "fn", Keywords.FN },
    .{ "if", Keywords.IF },
    .{ "else", Keywords.ELSE },
    .{ "var", Keywords.VAR },
    .{ "then", Keywords.THEN },
    .{ "end", Keywords.END },
    .{ "true", Keywords.TRUE },
    .{ "false", Keywords.FALSE },
    .{ "nil", Keywords.NIL },
    .{ "return", Keywords.RETURN },
    .{ "for", Keywords.FOR },
    .{ "while", Keywords.WHILE },
});

pub const Position = struct {
    column: usize = 0,
    line: usize = 0,
    filename: []const u8,
};

pub const Token = struct {
    token_type: Tokens,
    literal: []const u8,
    position: Position = undefined, // Just to make our life easir during testing, this way we dont need to pass in a position every time we create a token by hand

    pub inline fn makeToken(token_t: Tokens, literal: []const u8, position: Position) Token {
        return Token{ .token_type = token_t, .literal = literal, .position = position };
    }

    pub inline fn makeIllegalToken(message: []const u8, position: Position) Token {
        return Token{ .token_type = Tokens.ILLEGAL, .literal = message, .position = position };
    }
};

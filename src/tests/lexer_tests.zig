const std = @import("std");
const expect = std.testing.expect;
const lexer = @import("lexer").Lexer;

test "make lexer" {
    const input: []const u8 = "test input";
    const l: lexer = lexer.init(input);

    try expect(l.content == input);
    try expect(@typeName(l) == @typeName(lexer));
}

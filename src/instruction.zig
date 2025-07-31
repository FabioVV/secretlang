const std = @import("std");
const mem = std.mem;

const token = @import("token.zig");
const TokenType = token.Tokens;

pub const Instruction = u32; // Our bytecode is 32 bits in size

inline fn I(value: anytype) u32 { // Convenience function to make the code below less verbose
    return @as(u32, value);
}

pub const Opcode = enum(u8) {
    OP_LOADK = 0,
    OP_LOADKL = 1,

    OP_ADD = 2,
    OP_SUB = 3,
    OP_MUL = 4,
    OP_DIV = 5,

    OP_TRUE = 6,
    OP_FALSE = 7,
    OP_NIL = 8,

    OP_EQUAL = 9,
    OP_NOTEQUAL = 10,
    OP_GREATERTHAN = 11,
    OP_LESSTHAN = 12,
    OP_LESSEQUAL = 13,
    OP_GREATEREQUAL = 14,

    OP_MINUS = 15,
    OP_BANG = 16,

    OP_JUMP_IF_FALSE = 17,
    OP_JUMP = 18,

    OP_SET_GLOBAL = 19,
    OP_GET_GLOBAL = 20,

    OP_SET_LOCAL = 21,
    OP_GET_LOCAL = 22,

    OP_RETURN = 23,
    OP_RETURN_N = 24,
};

// Maybe make a struct InstructionHandler to encode/decode

pub inline fn GET_OPCODE(instruction: Instruction) Opcode {
    return @enumFromInt((instruction >> 26) & 0x3F);
}

pub inline fn ENCODE_DEFINE_GLOBAL(r_dest: u8, constantIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.OP_SET_GLOBAL)) << 26 | (I(r_dest) << 18) | I(constantIndex);
}

pub inline fn ENCODE_GET_GLOBAL(r_dest: u8, constantIdenIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.OP_GET_GLOBAL)) << 26 | (I(r_dest) << 18) | I(constantIdenIndex);
}

pub inline fn ENCODE_BINARY(operator: TokenType, r_dest: u8, ra: u8, rb: u8) Instruction {
    const opcode: Opcode = switch (operator) {
        .PLUS => .OP_ADD,
        .MINUS => .OP_SUB,
        .ASTERISK => .OP_MUL,
        .FSLASH => .OP_DIV,
        .NOT_EQUAL => .OP_NOTEQUAL,
        .EQUAL_EQUAL => .OP_EQUAL,
        .GREATERT => .OP_GREATERTHAN,
        .LESST => .OP_LESSTHAN,
        .LESS_EQUAL => .OP_LESSEQUAL,
        .GREATER_EQUAL => .OP_GREATEREQUAL,
        else => return 0,
    };

    return I(@intFromEnum(opcode)) << 26 | (I(r_dest) << 18) | (I(ra) << 10) | rb;
}

pub inline fn ENCODE_PREFIX(operator: []const u8, r_dest: u8, ra: u8) Instruction {
    switch (operator[0]) {
        '!' => {
            return I(@intFromEnum(Opcode.OP_BANG)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
        },
        '-' => {
            return I(@intFromEnum(Opcode.OP_MINUS)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
        },
        else => {
            return 0;
        },
    }
}

pub inline fn ENCODE_BOOLEAN_TRUE(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.OP_TRUE)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_BOOLEAN_FALSE(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.OP_FALSE)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_NIL(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.OP_NIL)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_JUMP_IF_FALSE(r_dest: u8) Instruction { // The instrutions here is incomplete, there is 18 bits which are set during compile time
    return I(@intFromEnum(Opcode.OP_JUMP_IF_FALSE)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_JUMP() Instruction { // The instrutions here is incomplete, there is 18 bits which are set during compile time
    return I(@intFromEnum(Opcode.OP_JUMP)) << 26;
}

pub inline fn ENCODE_LOADK(r_dest: u8, constantIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.OP_LOADK)) << 26 | (I(r_dest) << 18) | I(constantIndex);
}

pub inline fn ENCODE_RETURN(r_dest: u8, ra: u8) Instruction {
    return I(@intFromEnum(Opcode.OP_RETURN)) << 26 | (I(r_dest) << 18) | I(ra);
}

pub inline fn ENCODE_RETURN_N(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.OP_RETURN_N)) << 26 | (I(r_dest) << 18);
}

pub inline fn DECODE_JUMP_OFFSET(instruction: Instruction) usize {
    return @intCast((instruction & 0x3FFFF));
}

pub inline fn DECODE_CONSTANT_IDX(instruction: Instruction) u16 {
    return @intCast(instruction & 0x1FFFF);
}

pub inline fn DECODE_RA(instruction: Instruction) u8 {
    return @intCast((instruction >> 10) & 0xFF);
}

pub inline fn DECODE_RB(instruction: Instruction) u8 {
    return @intCast(instruction & 0xFF);
}

pub inline fn DECODE_RC(instruction: Instruction) u8 {
    return @intCast((instruction >> 18) & 0xFF);
}

const std = @import("std");
const mem = std.mem;

const token = @import("token.zig");
const TokenType = token.Tokens;

pub const Instruction = u32;

inline fn I(value: anytype) u32 { // Convenience function to make the code below less verbose
    return @as(u32, value);
}

pub const Opcode = enum(u8) {
    LOADK = 0, // loads an index of a constant into register RC
    LOADKL = 1, // loads an long index(> 65555) of a constant into register RC   TODO: IMPLEMENT THIS

    LOADI = 2, // loads an integer into register RC
    LOADT = 3, // loads TRUE into register RC
    LOADF = 4, // loads FALSE into register RC
    LOADN = 5, // loads NIL into register RC


    ADD = 6,
    SUB = 7,
    MUL = 8,
    DIV = 9,

    ADDI = 10,
    SUBI = 11,
    MULI = 12,
    DIVI = 13,

    EQUAL = 14,
    NOTEQUAL = 15,
    GREATERTHAN = 16,
    LESSTHAN = 17,
    LESSEQUAL = 18,
    GREATEREQUAL = 19,

    MINUS = 20,
    BANG = 21,

    JMPF = 22,
    JMP = 23,

    SGLOBAL = 24,
    GGLOBAL = 25,

    CALL = 26,
    RET = 27,
    RETN = 28,

    MOVE = 29,
    PUSH = 30,
};

// Maybe make a struct InstructionHandler to encode/decode

pub inline fn GET_OPCODE(instruction: Instruction) Opcode {
    return @enumFromInt((instruction >> 26) & 0x3F);
}

pub inline fn ENCODE_DEFINE_GLOBAL(r_dest: u8, constantIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.SGLOBAL)) << 26 | (I(r_dest) << 18) | I(constantIndex);
}

pub inline fn ENCODE_GET_GLOBAL(r_dest: u8, constantIdenIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.GGLOBAL)) << 26 | (I(r_dest) << 18) | I(constantIdenIndex);
}

pub inline fn ENCODE_BINARY(operator: TokenType, r_dest: u8, ra: u8, rb: u8) Instruction {
    const opcode: Opcode = switch (operator) {
        .PLUS => .ADD,
        .MINUS => .SUB,
        .ASTERISK => .MUL,
        .FSLASH => .DIV,
        .NOT_EQUAL => .NOTEQUAL,
        .EQUAL_EQUAL => .EQUAL,
        .GREATERT => .GREATERTHAN,
        .LESST => .LESSTHAN,
        .LESS_EQUAL => .LESSEQUAL,
        .GREATER_EQUAL => .GREATEREQUAL,
        else => return 0,
    };

    return I(@intFromEnum(opcode)) << 26 | (I(r_dest) << 18) | (I(ra) << 10) | rb;
}

pub inline fn ENCODE_PREFIX(operator: []const u8, r_dest: u8, ra: u8) Instruction {
    switch (operator[0]) {
        '!' => {
            return I(@intFromEnum(Opcode.BANG)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
        },
        '-' => {
            return I(@intFromEnum(Opcode.MINUS)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
        },
        else => {
            return 0;
        },
    }
}

pub inline fn ENCODE_BOOLEAN_TRUE(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.LOADT)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_BOOLEAN_FALSE(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.LOADF)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_NIL(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.LOADN)) << 26 | (I(r_dest) << 18);
}
/// Generates a incomplete jump instruction with 18 free bits, where during compilation it must be completed
pub inline fn ENCODE_JUMP_IF_FALSE(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.JMPF)) << 26 | (I(r_dest) << 18);
}

/// Generates a incomplete jump instruction with 18 free bits, where during compilation it must be completed
pub inline fn ENCODE_JUMP() Instruction {
    return I(@intFromEnum(Opcode.JMP)) << 26;
}

pub inline fn ENCODE_LOADK(r_dest: u8, constantIndex: u16) Instruction {
    return I(@intFromEnum(Opcode.LOADK)) << 26 | (I(r_dest) << 18) | I(constantIndex);
}

pub inline fn ENCODE_MOVE(r_dest: u8, ra: u16) Instruction {
    return I(@intFromEnum(Opcode.MOVE)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
}

pub inline fn ENCODE_RETURN(r_dest: u8) Instruction {
    return I(@intFromEnum(Opcode.RET)) << 26 | (I(r_dest) << 18);
}

pub inline fn ENCODE_RETURN_N() Instruction {
    return I(@intFromEnum(Opcode.RETN)) << 26;
}

pub inline fn ENCODE_CALL(r_dest: u8, ra: u8) Instruction {
    return I(@intFromEnum(Opcode.CALL)) << 26 | (I(r_dest) << 18) | (I(ra) << 10);
}

pub inline fn ENCODE_PUSH(r_from: u8) Instruction {
    return I(@intFromEnum(Opcode.PUSH)) << 26 | (I(r_from) << 18);

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

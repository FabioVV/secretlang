const std = @import("std");

pub const Instruction = u32; // Our bytecode is 32 bits in size

pub const Opcode = enum(u8) {
    OP_CONSTANT = 0,
    OP_LOAD = 1,

    OP_ADD = 2,
    OP_SUB = 3,
    OP_MUL = 4,
    OP_DIV = 5,
};

// Maybe make a struct InstructionHandler to encode/decode

pub fn GET_OPCODE(instruction: Instruction) Opcode {
    return @enumFromInt((instruction >> 26) & 0x3F);
}

pub fn ENCODE_CONSTANT(constantIndex: u16, r_dest: u8) Instruction {
    return @as(Instruction, @intFromEnum(Opcode.OP_CONSTANT)) << 26 | (@as(Instruction, r_dest) << 18) | @as(Instruction, constantIndex);
}

pub fn ENCODE_BINARY(operator: u8, r_dest: u8, ra: u8, rb: u8) Instruction {
    switch (operator) {
        '+' => {
            return @as(Instruction, @intFromEnum(Opcode.OP_ADD)) << 26 | (@as(Instruction, r_dest) << 18) | (@as(Instruction, ra) << 10) | rb;
        },
        '-' => {
            return @as(Instruction, @intFromEnum(Opcode.OP_SUB)) << 26 | (@as(Instruction, r_dest) << 18) | (@as(Instruction, ra) << 10) | rb;
        },
        '*' => {
            return @as(Instruction, @intFromEnum(Opcode.OP_MUL)) << 26 | (@as(Instruction, r_dest) << 18) | (@as(Instruction, ra) << 10) | rb;
        },
        '/' => {
            return @as(Instruction, @intFromEnum(Opcode.OP_DIV)) << 26 | (@as(Instruction, r_dest) << 18) | (@as(Instruction, ra) << 10) | rb;
        },
        else => {
            return 0;
        },
    }
}

pub fn DECODE_CONSTANT_IDX(instruction: Instruction) u16 {
    return @intCast(instruction & 0x1FFFF);
}

pub fn DECODE_RA(instruction: Instruction) u8 {
    return @intCast((instruction >> 10) & 0xFF);
}

pub fn DECODE_RB(instruction: Instruction) u8 {
    return @intCast(instruction & 0xFF);
}

pub fn DECODE_RC(instruction: Instruction) u8 {
    return @intCast((instruction >> 18) & 0xFF);
}

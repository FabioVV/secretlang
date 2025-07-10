const std = @import("std");
const expect = std.testing.expect;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

pub const TOTAL_REGISTERS = 256;

pub const VM = struct {
    registers: std.BoundedArray(Value, TOTAL_REGISTERS),
    arena: std.heap.ArenaAllocator,
    instructions: *std.ArrayList(Instruction),
    constantsPool: *std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();
    }

    fn GET_CONSTANT(self: *VM, idx: u16) Value {
        return self.constantsPool.*.items[idx];
    }

    fn BINARY_OP(self: *VM, operator: u8, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (operator) {
            '+' => {
                self.registers.set(RC, Value.createNumber(RB.NUMBER + RA.NUMBER));
            },
            '-' => {
                self.registers.set(RC, Value.createNumber(RB.NUMBER - RA.NUMBER));
            },
            '*' => {
                self.registers.set(RC, Value.createNumber(RB.NUMBER * RA.NUMBER));
            },
            '/' => {
                self.registers.set(RC, Value.createNumber(RB.NUMBER / RA.NUMBER));
            },
            else => {},
        }

        const s = self.registers.get(RC).asNumber().?;
        std.debug.print("{d:6.2}\n", .{s});
    }

    pub fn run(self: *VM) void {
        for (self.instructions.*.items) |curInstruction| {
            const opcode = _instruction.GET_OPCODE(curInstruction);
            //std.debug.print("{any}\n", .{curInstruction});
            //std.debug.print("{any}\n", .{opcode});

            switch (opcode) {
                .OP_CONSTANT => {
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const contantValue = self.GET_CONSTANT(constantIdx);

                    self.registers.set(RC, contantValue);
                },
                .OP_ADD => {
                    self.*.BINARY_OP('+', curInstruction);
                },
                .OP_SUB => {
                    self.*.BINARY_OP('-', curInstruction);
                },
                .OP_MUL => {
                    self.*.BINARY_OP('*', curInstruction);
                },
                .OP_DIV => {
                    self.*.BINARY_OP('/', curInstruction);
                },
                else => {
                    std.debug.print("Unhandled OPCODE: {any} \n", .{opcode});
                    std.process.exit(1);
                },
            }
        }
    }
};

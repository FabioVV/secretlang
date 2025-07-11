const std = @import("std");
const expect = std.testing.expect;
const mem = std.mem;

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

//TODO: ADD RUNTIME AND COMPILE TIME ERRORS

pub const TOTAL_REGISTERS = 256;

pub const VM = struct {
    registers: std.BoundedArray(Value, TOTAL_REGISTERS),

    arena: std.heap.ArenaAllocator,

    instructions: *std.ArrayList(Instruction),
    constantsPool: *std.ArrayList(Value),

    pc: usize,

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;

        vm.*.pc = 0;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();
    }

    fn GET_CONSTANT(self: *VM, idx: u16) Value {
        return self.constantsPool.*.items[idx];
    }

    fn makeComparison(self: *VM, operator: []const u8, RA: Value, RB: Value, RC: u8) void {
        if (RA.isNumber() and RB.isNumber()) {
            if (mem.eql(u8, operator, "<")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER < RA.NUMBER));
            } else if (mem.eql(u8, operator, ">")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER > RA.NUMBER));
            } else if (mem.eql(u8, operator, "<=")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER <= RA.NUMBER));
            } else if (mem.eql(u8, operator, ">=")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER >= RA.NUMBER));
            } else if (mem.eql(u8, operator, "==")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER == RA.NUMBER));
            } else if (mem.eql(u8, operator, "!=")) {
                self.registers.set(RC, Value.createBoolean(RB.NUMBER != RA.NUMBER));
            }

            const s = self.registers.get(RC).BOOLEAN;
            std.debug.print("{}\n", .{s});
        } else if (RA.isBoolean() and RB.isBoolean()) {
            if (mem.eql(u8, operator, "==")) {
                self.registers.set(RC, Value.createBoolean(RB.BOOLEAN == RA.BOOLEAN));
            } else if (mem.eql(u8, operator, "!=")) {
                self.registers.set(RC, Value.createBoolean(RB.BOOLEAN != RA.BOOLEAN));
            }

            const s = self.registers.get(RC).BOOLEAN;
            std.debug.print("{}\n", .{s});
        }
    }

    fn BINARY_OP(self: *VM, operator: []const u8, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);
        //std.debug.print("RC: {d}\n", .{RC});

        if (mem.eql(u8, operator, "+")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER + RA.NUMBER));
        } else if (mem.eql(u8, operator, "-")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER - RA.NUMBER));
        } else if (mem.eql(u8, operator, "*")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER * RA.NUMBER));
        } else if (mem.eql(u8, operator, "/")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER / RA.NUMBER));
        }

        const s = self.registers.get(RC).NUMBER;
        std.debug.print("{d:6.2}\n", .{s});
    }

    pub fn run(self: *VM) void {
        while (self.*.pc < self.instructions.*.items.len) : (self.*.pc += 1) {
            const curInstruction = self.instructions.*.items[self.*.pc];
            const opcode = _instruction.GET_OPCODE(curInstruction);

            switch (opcode) {
                .OP_CONSTANT => {
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const contantValue = self.GET_CONSTANT(constantIdx);

                    self.registers.set(RC, contantValue);
                },
                .OP_ADD => {
                    self.*.BINARY_OP("+", curInstruction);
                },
                .OP_SUB => {
                    self.*.BINARY_OP("-", curInstruction);
                },
                .OP_MUL => {
                    self.*.BINARY_OP("*", curInstruction);
                },
                .OP_DIV => {
                    self.*.BINARY_OP("/", curInstruction);
                },
                .OP_EQUAL => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison("==", RA, RB, RC);
                },
                .OP_NOTEQUAL => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison("!=", RA, RB, RC);
                },
                .OP_LESSEQUAL => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison("<=", RA, RB, RC);
                },
                .OP_GREATEREQUAL => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison(">=", RA, RB, RC);
                },
                .OP_GREATERTHAN => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison(">", RA, RB, RC);
                },
                .OP_LESSTHAN => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RB = self.registers.get(_instruction.DECODE_RB(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.*.makeComparison("<", RA, RB, RC);
                },
                .OP_FALSE => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createBoolean(false));

                    const s = self.registers.get(RC).BOOLEAN;
                    std.debug.print("{}\n", .{s});
                },
                .OP_TRUE => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createBoolean(true));

                    const s = self.registers.get(RC).BOOLEAN;
                    std.debug.print("{}\n", .{s});
                },
                .OP_NIL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createNil());

                    const s = self.registers.get(RC).NIL;
                    std.debug.print("{}\n", .{s});
                },
                .OP_BANG => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.registers.set(RC, Value.createBoolean(!RA.BOOLEAN));

                    const s = self.registers.get(RC).BOOLEAN;
                    std.debug.print("{}\n", .{s});
                },
                .OP_MINUS => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.registers.set(RC, Value.createNumber(-RA.NUMBER));
                    const s = self.registers.get(RC).NUMBER;
                    std.debug.print("{d:6.2}\n", .{s});
                },
                .OP_JUMP_IF_FALSE => {
                    const RC = self.registers.get(_instruction.DECODE_RC(curInstruction));
                    const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);

                    if (!RC.isTruthy()) {
                        self.*.pc += jumpOffset;
                    }
                },
                .OP_JUMP => {
                    const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);
                    self.*.pc += jumpOffset;
                },

                else => {
                    std.debug.print("Unhandled OPCODE: {any} \n", .{opcode});
                    std.process.exit(1);
                },
            }
        }
    }
};

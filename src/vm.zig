const std = @import("std");
const expect = std.testing.expect;
const mem = std.mem;

const panic = @import("error.zig");
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

    globals: std.StringHashMap(Value),

    pc: usize,

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;
        vm.*.globals = std.StringHashMap(Value).init(arena.allocator());

        vm.*.pc = 0;

        return vm;
    }

    pub fn repl_init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction), globals: std.StringHashMap(Value)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;
        vm.*.globals = globals.clone() catch unreachable; // Clone the existing globals

        vm.*.pc = 0;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();
    }

    fn GET_CONSTANT(self: *VM, idx: u16) ?Value {
        if(idx >= self.constantsPool.*.items.len){
            return null;
        }
        return self.constantsPool.*.items[idx];
    }

    fn GET_CONSTANT_STRING(self: *VM, idx: u16) ?[]const u8 {
        const v = self.GET_CONSTANT(idx);
        if(v != null){
            return v.?.STRING;
        }

       return null;
    }

    fn makeComparison(self: *VM, operator: []const u8, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

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

            self.registers.get(RC).print();
        } else if (RA.isBoolean() and RB.isBoolean()) {
            if (mem.eql(u8, operator, "==")) {
                self.registers.set(RC, Value.createBoolean(RB.BOOLEAN == RA.BOOLEAN));
            } else if (mem.eql(u8, operator, "!=")) {
                self.registers.set(RC, Value.createBoolean(RB.BOOLEAN != RA.BOOLEAN));
            }

            self.registers.get(RC).print();
        }
    }

    fn BINARY_OP(self: *VM, operator: []const u8, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        if (mem.eql(u8, operator, "+")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER + RA.NUMBER));
        } else if (mem.eql(u8, operator, "-")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER - RA.NUMBER));
        } else if (mem.eql(u8, operator, "*")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER * RA.NUMBER));
        } else if (mem.eql(u8, operator, "/")) {
            self.registers.set(RC, Value.createNumber(RB.NUMBER / RA.NUMBER));
        }

        self.registers.get(RC).print();
    }

    pub fn run(self: *VM) void {
        while (self.*.pc < self.instructions.*.items.len) : (self.*.pc += 1) {
            const curInstruction = self.instructions.*.items[self.*.pc];
            const opcode = _instruction.GET_OPCODE(curInstruction);

            switch (opcode) {
                .OP_CONSTANT => {
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const contantValue = self.GET_CONSTANT(constantIdx).?;

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
                    self.*.makeComparison("==", curInstruction);
                },
                .OP_NOTEQUAL => {
                    self.*.makeComparison("!=", curInstruction);
                },
                .OP_LESSEQUAL => {
                    self.*.makeComparison("<=", curInstruction);
                },
                .OP_GREATEREQUAL => {
                    self.*.makeComparison(">=", curInstruction);
                },
                .OP_GREATERTHAN => {
                    self.*.makeComparison(">", curInstruction);
                },
                .OP_LESSTHAN => {
                    self.*.makeComparison("<", curInstruction);
                },
                .OP_FALSE => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createBoolean(false));

                    self.registers.get(RC).print();
                },
                .OP_TRUE => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createBoolean(true));

                    self.registers.get(RC).print();
                },
                .OP_NIL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.registers.set(RC, Value.createNil());

                    self.registers.get(RC).print();
                },
                .OP_BANG => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.registers.set(RC, Value.createBoolean(!RA.BOOLEAN));

                    self.registers.get(RC).print();
                },
                .OP_MINUS => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.registers.set(RC, Value.createNumber(-RA.NUMBER));

                    self.registers.get(RC).print();
                },
                .OP_JUMP_IF_FALSE => {
                    const RC = self.registers.get(_instruction.DECODE_RC(curInstruction));

                    if (!RC.isTruthy()) {
                        const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);
                        self.*.pc += jumpOffset;
                    }
                },
                .OP_JUMP => {
                    const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);
                    self.*.pc += jumpOffset;
                },
                .OP_SET_GLOBAL => {
                    const RC = self.registers.get(_instruction.DECODE_RC(curInstruction));
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const identifier = self.GET_CONSTANT_STRING(constantIdx).?;

                    self.*.globals.put(identifier, RC) catch |err|{
                        panic.exitWithError("Error trying to store global variable", err);
                    };

                },
                .OP_GET_GLOBAL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const identifier = self.GET_CONSTANT_STRING(constantIdx).?;

                    var identifierValue: Value = undefined;
                    if(self.*.globals.get(identifier)) |v|{
                        identifierValue = v;

                    } else {
                        // runtime error, variable not defined
                        std.debug.print("Undefined variable: {s} {d}\n", .{identifier, self.globals.count()});
                        std.process.exit(1);

                    }

                    self.registers.set(RC, identifierValue);

                    self.registers.get(RC).print();
                },
                else => {
                    std.debug.print("Unhandled OPCODE: {any} \n", .{opcode});
                    std.process.exit(1);

                },
            }
        }
    }
};

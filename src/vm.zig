const std = @import("std");
const expect = std.testing.expect;
const mem = std.mem;

const dbg = @import("debug.zig");
const panic = @import("error.zig");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

pub const TOTAL_REGISTERS = 255;

pub const VM = struct {
    registers: std.BoundedArray(Value, TOTAL_REGISTERS),

    arena: std.heap.ArenaAllocator,

    instructions: *std.ArrayList(Instruction),
    instructions_positions: *std.AutoHashMap(u32, Position),

    constantsPool: *std.ArrayList(Value),


    globals: std.StringHashMap(Value),

    pc: usize,

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction), instructions_positions: *std.AutoHashMap(u32, Position)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;
        vm.*.instructions_positions = instructions_positions;
        vm.*.globals = std.StringHashMap(Value).init(arena.allocator());

        vm.*.pc = 0;

        return vm;
    }

    pub fn repl_init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction), instructions_positions: *std.AutoHashMap(u32, Position), globals: std.StringHashMap(Value)) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.*.arena = arena;
        vm.registers = std.BoundedArray(Value, TOTAL_REGISTERS).init(TOTAL_REGISTERS) catch unreachable;
        vm.*.instructions = instructions;
        vm.*.constantsPool = constantsPool;
        vm.*.instructions_positions = instructions_positions;

        vm.*.globals = globals.clone() catch unreachable; // Clone the existing globals

        vm.*.pc = 0;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.instructions.deinit();
        self.instructions_positions.deinit();
        self.constantsPool.deinit();
        self.arena.deinit();
    }

    /// Emits a runtime error
    fn rError(self: *VM, comptime message:[] const u8, varargs: anytype) void {
        const pos = self.instructions_positions.get( @intCast(self.pc)).?;
        std.debug.print(" {s}In [{s}] {d}:{d}{s}: \n", .{ dbg.ANSI_CYAN, pos.filename, pos.line, pos.column, dbg.ANSI_RESET });

        std.debug.print(" {s}runtime error{s}: ", .{ dbg.ANSI_RED, dbg.ANSI_RESET });
        std.debug.print(message, varargs);
        std.debug.print("\n", .{});
    }

    inline fn GET_CONSTANT(self: *VM, idx: u16) ?Value {
        if(idx >= self.constantsPool.*.items.len){
            return null;
        }
        return self.constantsPool.*.items[idx];
    }

    inline fn GET_CONSTANT_STRING(self: *VM, idx: u16) ?[]const u8 {
        const v = self.GET_CONSTANT(idx);
        if(v != null){
            switch (v.?.OBJECT.*) {
                .STRING => |str| {
                    return str.chars;
                }
            }
        }

       return null;
    }

    inline fn cmpEqual(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| b == a,
                else => false,
            },
            .BOOLEAN => |a| switch (RB) {
                .BOOLEAN => |b| b == a,
                else => false,
            },
            .OBJECT => |a| switch (a.*) {
                .STRING => |str_a| if (RB.asZigString()) |str_b|
                std.mem.eql(u8, str_b, str_a.chars)
                else
                    false,
            },
            .NIL => switch (RB) {
                .NIL => true,
                else => false,
            },
        };

        self.registers.set(RC, Value.createBoolean(result));
        self.registers.get(RC).print();
    }

    inline fn cmpNotEqual(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| b != a,
                else => true,
            },
            .BOOLEAN => |a| switch (RB) {
                .BOOLEAN => |b| b != a,
                else => true,
            },
            .OBJECT => |a| switch (a.*) {
                .STRING => |str_a| if (RB.asZigString()) |str_b|
                !std.mem.eql(u8, str_b, str_a.chars)
                else
                    true,
            },
            .NIL => switch (RB) {
                .NIL => false,
                else => true,
            },
        };

        self.registers.set(RC, Value.createBoolean(result));
        self.registers.get(RC).print();
    }

    inline fn cmpLessThan(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createBoolean(b < a));
                    self.registers.get(RC).print();
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn cmpGreaterThan(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createBoolean(b > a));
                    self.registers.get(RC).print();

                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn cmpLessEqual(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createBoolean(b <= a));
                    self.registers.get(RC).print();

                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }

    }

    inline fn cmpGreaterEqual(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createBoolean(b >= a));
                    self.registers.get(RC).print();

                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn mathAdd(self: *VM, instruction: Instruction) void{ // add string concatenation
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createNumber(b + a));
                    self.registers.get(RC).print();
                },
                else => |p| {
                    self.rError("type error: operands must be both numeric or string, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            .OBJECT => |a| switch (a.*) {
                .STRING => |str_a| {
                    if(RB.asZigString()) |str_b|{
                        const result = std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ str_b, str_a.chars }) catch {
                            self.rError("out of memory during string concatenation", .{});
                            std.process.exit(1);
                        };

                        const value = Value.createString(self.arena.allocator(), result);// need to fix memory for gc stuff


                        self.registers.set(RC, value);
                        self.registers.get(RC).print();
                    } else {
                        self.rError("type error: operands must be both numeric or string, got {s}", .{@tagName(RB)});
                        std.process.exit(1);
                    }

                },

            },
            else => |p| {
                self.rError("type error: operands must be both numeric or string, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn mathSub(self: *VM, instruction: Instruction) void{
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createNumber(b - a));
                    self.registers.get(RC).print();
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn mathMul(self: *VM, instruction: Instruction) void{ // add string multiplication
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createNumber(b * a));
                    self.registers.get(RC).print();
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
    }

    inline fn mathDiv(self: *VM, instruction: Instruction) void{
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    if(b == 0) {
                        // division by zero error
                    } else {
                        self.registers.set(RC, Value.createNumber(b / a));
                        self.registers.get(RC).print();
                    }
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                std.process.exit(1);
            },
        }
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
                    self.*.mathAdd(curInstruction);
                },
                .OP_SUB => {
                    self.*.mathSub(curInstruction);
                },
                .OP_MUL => {
                    self.*.mathMul(curInstruction);
                },
                .OP_DIV => {
                    self.*.mathDiv(curInstruction);
                },
                .OP_EQUAL => {
                    self.*.cmpEqual(curInstruction); // ==
                },
                .OP_NOTEQUAL => {
                    self.*.cmpNotEqual(curInstruction); // !=
                },
                .OP_LESSEQUAL => {
                    self.*.cmpLessEqual(curInstruction); // <=
                },
                .OP_GREATEREQUAL => {
                    self.*.cmpGreaterEqual(curInstruction); // >=
                },
                .OP_GREATERTHAN => {
                    self.*.cmpGreaterThan(curInstruction); // >
                },
                .OP_LESSTHAN => {
                    self.*.cmpLessThan(curInstruction); // <
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


                    switch (RA) {
                        .BOOLEAN => |n| self.registers.set(RC, Value.createBoolean(!n)),
                        else => |p| {
                            self.rError("type error: operand must be boolean, got {s}", .{@tagName(p)});
                            std.process.exit(1);
                        },
                    }

                    self.registers.get(RC).print();
                },
                .OP_MINUS => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    switch (RA) {
                        .NUMBER => |n| self.registers.set(RC, Value.createNumber(-n)),
                        else => |p| {
                            self.rError("type error: operand must be numeric, got {s}", .{@tagName(p)});
                            std.process.exit(1);
                        },
                    }

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
                        self.rError("undefined variable: {s}", .{identifier});
                        std.process.exit(1);
                    }

                    self.registers.set(RC, identifierValue);

                    self.registers.get(RC).print();
                },
                else => {
                    self.rError("Unhandled OPCODE: {any} \n", .{opcode});
                    std.process.exit(1);

                },
            }
        }
    }
};

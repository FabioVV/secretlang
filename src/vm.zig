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
const Object = _value.Object;
const String = _value.String;

const NIL = Value{ .NIL = void{} };
const Instruction = _instruction.Instruction;

pub const MAX_REGISTERS = 255;
pub const MAX_GLOBALS = 65535;

pub const VM = struct {
    registers: std.BoundedArray(Value, MAX_REGISTERS),

    arena: std.heap.ArenaAllocator,

    source: *[]const u8,
    instructions: *std.ArrayList(Instruction),
    instructions_positions: *std.AutoHashMap(u32, Position),

    constantsPool: *std.ArrayList(Value),
    globals: *std.BoundedArray(Value, MAX_GLOBALS),

    objects: ?*Object,
    strings: *std.StringHashMap(Value),

    pc: usize,

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction), instructions_positions: *std.AutoHashMap(u32, Position), source: *[]const u8, strings: *std.StringHashMap(Value), objects: ?*Object) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.pc = 0;

        vm.source = source;
        vm.arena = arena;
        vm.registers = std.BoundedArray(Value, MAX_REGISTERS).init(MAX_REGISTERS) catch unreachable;
        vm.instructions = instructions;
        vm.constantsPool = constantsPool;
        vm.instructions_positions = instructions_positions;
        vm.globals = std.BoundedArray(Value, MAX_GLOBALS).init(MAX_GLOBALS) catch unreachable;
        vm.strings = strings;
        vm.objects = objects;

        return vm;
    }

    pub fn repl_init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), instructions: *std.ArrayList(Instruction), instructions_positions: *std.AutoHashMap(u32, Position), globals: *std.BoundedArray(Value, MAX_GLOBALS), source: *[]const u8, strings: *std.StringHashMap(Value), objects: ?*Object) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.source = source;
        vm.arena = arena;
        vm.registers = std.BoundedArray(Value, MAX_REGISTERS).init(MAX_REGISTERS) catch unreachable;
        vm.instructions = instructions;
        vm.constantsPool = constantsPool;
        vm.instructions_positions = instructions_positions;

        vm.globals = globals;
        vm.strings = strings;
        vm.objects = objects;

        vm.pc = 0;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        //         var current = self.objects;
        //         while(current) |obj|{
        //             const next = obj.next;
        //
        //             switch (obj.*.data) {
        //                 .STRING => |str| {
        //                     self.arena.allocator().free(str.chars);
        //                     self.arena.allocator().destroy(str);
        //                 }
        //             }
        //
        //             self.arena.allocator().destroy(obj);
        //             current = next;
        //         }

        self.arena.deinit();
    }

    /// Emits a runtime error
    fn rError(self: *VM, comptime message: []const u8, varargs: anytype) void {
        const pos = self.instructions_positions.get(@intCast(self.pc)).?;

        const source = dbg.getSourceLineFromPosition(self.source.*, pos);

        const runtimeErrMsg = std.fmt.allocPrint(self.arena.allocator(), message, varargs) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgLocation = std.fmt.allocPrint(self.arena.allocator(), "{s}In [{s}] {d}:{d}{s}", .{ dbg.ANSI_CYAN, pos.filename, pos.line, pos.column, dbg.ANSI_RESET }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgErrorInfo = std.fmt.allocPrint(self.arena.allocator(), "{s}runtime error{s}: {s}", .{ dbg.ANSI_RED, dbg.ANSI_RESET, runtimeErrMsg }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgSource = std.fmt.allocPrint(self.arena.allocator(), "{s}", .{source}) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        var msgt: []u8 = &[_]u8{};
        for (1..source.len + 1) |idx| {
            //std.debug.print("{d} : {d}\n", .{ idx, pos.column });
            if (idx == pos.column + 1) {
                msgt = std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ msgt, "^" }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            } else {
                msgt = std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ msgt, " " }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            }
        }

        std.debug.print("\n\n-> {s}\n {d} | {s}\n   | {s}\n   | {s}", .{ msgLocation, pos.line, msgSource, msgt, msgErrorInfo });

        //std.debug.print(message, varargs);
        std.debug.print("\n", .{});
    }

    inline fn GET_CONSTANT(self: *VM, idx: u16) ?Value {
        if (idx >= self.constantsPool.*.items.len) {
            return null;
        }
        return self.constantsPool.*.items[idx];
    }

    inline fn GET_CONSTANT_STRING(self: *VM, idx: u16) ?[]const u8 {
        const v = self.GET_CONSTANT(idx);
        if (v != null) {
            switch (v.?.OBJECT.data) {
                .STRING => |str| {
                    return str.chars;
                },
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
            .OBJECT => |a| switch (a.data) {
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
            .OBJECT => |a| switch (a.data) {
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
            },
        }
    }

    inline fn mathAdd(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    self.registers.set(RC, Value.createNumber(b + a));
                    self.registers.get(RC).print();
                },
                .OBJECT => |o| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(o.data), @tagName(RA) });
                },
                else => |p| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(p), @tagName(RA) });
                    //std.process.exit(1);
                },
            },
            .OBJECT => |a| switch (a.data) {
                .STRING => |str_a| {
                    if (RB.asZigString()) |str_b| {
                        const result = std.heap.page_allocator.alloc(u8, str_b.len + str_a.chars.len) catch unreachable;
                        @memcpy(result[0..str_b.len], str_b);
                        @memcpy(result[str_b.len..], str_a.chars);

                        const value = Value.kidnapString(std.heap.page_allocator, result, self.strings, &self.objects);

                        self.registers.set(RC, value);
                        self.registers.get(RC).print();
                    } else {
                        self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(RB), @tagName(a.data) });
                        //std.process.exit(1);

                    }
                },
            },
            else => |p| {
                self.rError("type error: operands must be both numeric or string, got {s}", .{@tagName(p)});
                //std.process.exit(1);
            },
        }
    }

    inline fn mathSub(self: *VM, instruction: Instruction) void {
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
            },
        }
    }

    inline fn mathMul(self: *VM, instruction: Instruction) void { // add string multiplication
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
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
            },
        }
    }

    inline fn mathDiv(self: *VM, instruction: Instruction) void {
        const RA = self.registers.get(_instruction.DECODE_RA(instruction));
        const RB = self.registers.get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .NUMBER => |a| switch (RB) {
                .NUMBER => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.registers.set(RC, Value.createNumber(b / a));
                        self.registers.get(RC).print();
                    }
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    //std.process.exit(1);
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                //std.process.exit(1);
            },
        }
    }

    pub fn run(self: *VM) void {
        while (self.*.pc < self.instructions.*.items.len) : (self.*.pc += 1) {
            const curInstruction = self.instructions.*.items[self.*.pc];
            const opcode = _instruction.GET_OPCODE(curInstruction);

            switch (opcode) {
                .OP_LOADK => {
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const contantValue = self.GET_CONSTANT(constantIdx).?;

                    std.debug.print("LOADK'\n", .{});

                    self.registers.set(RC, contantValue);
                    self.registers.get(RC).print();
                },
                .OP_ADD => {
                    std.debug.print("SUM'\n", .{});

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
                            //std.process.exit(1);
                        },
                    }

                    self.registers.get(RC).print();
                },
                .OP_MINUS => {
                    const RA = self.registers.get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);
                    std.debug.print("MINUS'\n", .{});

                    switch (RA) {
                        .NUMBER => |n| self.registers.set(RC, Value.createNumber(-n)),
                        else => |p| {
                            self.rError("type error: operand must be numeric, got {s}", .{@tagName(p)});
                            //std.process.exit(1);
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
                    const globalIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);

                    self.globals.slice()[globalIdx] = RC;
                },
                .OP_GET_GLOBAL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const globalIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);

                    self.registers.set(RC, self.globals.slice()[globalIdx]);
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

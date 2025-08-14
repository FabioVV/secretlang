const std = @import("std");
const expect = std.testing.expect;
const mem = std.mem;

const dbg = @import("debug.zig");
const errh = @import("error.zig");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const CompilationScope = @import("compiler.zig").CompilationScope;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const _nativef = @import("nativef.zig");
const Value = _value.Value;
const Object = _value.Object;
const String = _value.String;
const FunctionExpr = _value.FunctionExpr;
const Instruction = _instruction.Instruction;

pub const MAX_REGISTERS = 255;
pub const MAX_GLOBALS = 2048;
//pub const MAX_GLOBALS = 65535;
pub const MAX_FRAMES = 1024;
pub const STACK_SIZE = 2048;

pub const NIL = Value{ .NIL = void{} };
pub const TRUE = Value{ .BOOLEAN = true };
pub const FALSE = Value{ .BOOLEAN = false };

const CallFrame = struct {
    function: *FunctionExpr,
    registers: std.BoundedArray(Value, MAX_REGISTERS),
    pc: usize,
    return_register: u8,

    pub fn init(function: *FunctionExpr, return_register: u8) CallFrame {
        return CallFrame{
            .pc = 0,
            .function = function,
            .registers = std.BoundedArray(Value, MAX_REGISTERS).init(MAX_REGISTERS) catch unreachable,
            .return_register = return_register,
        };
    }

    pub inline fn instructions(self: *CallFrame) []Instruction {
        return self.function.instructions.items;
    }

    pub inline fn instructionsPositions(self: *CallFrame) std.AutoHashMap(u32, Position) {
        return self.function.instructions_positions;
    }
};

pub const VM = struct {
    stack: std.BoundedArray(Value, STACK_SIZE),
    frames: std.BoundedArray(CallFrame, MAX_FRAMES),

    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    source: *[]const u8,

    constantsPool: *std.ArrayList(Value),
    globals: std.BoundedArray(Value, MAX_GLOBALS),

    objects: ?*Object,
    strings: *std.StringHashMap(Value),

    frameIndex: usize,

    pub fn init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), compiledInst: *CompilationScope, source: *[]const u8, strings: *std.StringHashMap(Value), objects: ?*Object) *VM {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const vm = arena.allocator().create(VM) catch unreachable;

        vm.frameIndex = 1;

        vm.source = source;
        vm.arena = arena;
        vm.allocator = vm.arena.?.allocator();

        vm.stack = std.BoundedArray(Value, STACK_SIZE).init(0) catch unreachable;
        vm.frames = std.BoundedArray(CallFrame, MAX_FRAMES).init(MAX_FRAMES) catch unreachable;

        //vm.compiledInstructions = compiledInst;
        vm.constantsPool = constantsPool;

        vm.globals = std.BoundedArray(Value, MAX_GLOBALS).init(MAX_GLOBALS) catch unreachable;
        vm.strings = strings;
        vm.objects = objects;

        const mainFunction = Value.createFunctionExpr(vm.allocator, compiledInst.*, null, &vm.objects);
        vm.frames.set(1, CallFrame.init(mainFunction.asFunctionExpr().?, 0));

        for (_nativef.native_functions, 0..) |_, i| {
            vm.defineNative(@as(u16, @intCast(i)));
        }

        return vm;
    }

    pub fn repl_init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), compiledInst: *CompilationScope, globals: *std.BoundedArray(Value, MAX_GLOBALS), source: *[]const u8, strings: *std.StringHashMap(Value), objects: ?*Object) *VM {
        const vm = allocator.create(VM) catch unreachable;

        vm.frameIndex = 1;

        vm.source = source;
        vm.arena = null;
        vm.allocator = allocator;

        vm.stack = std.BoundedArray(Value, STACK_SIZE).init(0) catch unreachable;
        vm.frames = std.BoundedArray(CallFrame, MAX_FRAMES).init(MAX_FRAMES) catch unreachable;

        //vm.compiledInstructions = compiledInst;
        vm.constantsPool = constantsPool;

        vm.globals = globals.*;
        vm.strings = strings;
        vm.objects = objects;

        const mainFunction = Value.createFunctionExpr(vm.allocator, compiledInst.*, null, &vm.objects);
        vm.frames.set(1, CallFrame.init(mainFunction.asFunctionExpr().?, 0));

        for (_nativef.native_functions, 0..) |_, i| {
            vm.defineNative(@as(u16, @intCast(i)));
        }

        return vm;
    }

    pub fn deinit(self: *VM) void {
        //         var current = self.objects;
        //         while(current) |obj|{
        //             const next = obj.next;
        //
        //             switch (obj.*.data) {
        //                 .STRING => |str| {
        //                     self.allocator.free(str.chars);
        //                     self.allocator.destroy(str);
        //                 }
        //             }
        //
        //             self.allocator.destroy(obj);
        //             current = next;
        //         }
        if (self.arena) |arena| {
            arena.deinit();
        }
    }

    /// Emits a runtime error
    fn rError(self: *VM, comptime message: []const u8, varargs: anytype) void {
        const pos = self.currentCallFrame().instructionsPositions().get(@intCast(self.currentCallFrame().pc - 1)).?;
        const source = dbg.getSourceLine(self.source.*, pos);

        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, pos, source);
        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);

        const runtimeErrMsg = std.fmt.allocPrint(self.allocator, message, varargs) catch |err| {
            errh.exitWithError("unrecoverable error trying to runtime error message", err);
        };

        const errMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| runtime error: {s}
            \\
            \\
        , .{ pos.filename, pos.line, pos.column, pos.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, runtimeErrMsg }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write runtime error full message", err);
        };

        errh.printError(errMsg);
    }

    inline fn defineNative(self: *VM, globalSlot: u16) void {
        self.globals.slice()[globalSlot] = Value.getNative(globalSlot);
    }

    inline fn currentCallFrame(self: *VM) *CallFrame {
        return &self.frames.slice()[self.frameIndex];
    }

    inline fn currentCallFrameRegisters(self: *VM) *std.BoundedArray(Value, MAX_REGISTERS) {
        return &self.frames.slice()[self.frameIndex].registers;
    }

    inline fn pushCallFrame(self: *VM, frame: CallFrame) void {
        self.frameIndex += 1;
        self.frames.set(self.frameIndex, frame);
    }

    inline fn popCallFrame(self: *VM) CallFrame {
        self.frameIndex -= 1;
        return self.frames.get(self.frameIndex + 1);
    }

    inline fn push(self: *VM, v: Value) void {
        self.stack.append(v) catch {
            self.rError("stack overflow", .{});
        };
    }

    inline fn pop(self: *VM) ?Value {
        if (self.stack.len == 0) {
            self.rError("stack underflow", .{});
            return NIL;
        }
        return self.stack.pop();
    }

    inline fn GET_CONSTANT(self: *VM, idx: u16) ?Value {
        return if (idx >= self.constantsPool.items.len) null else self.constantsPool.items[idx];
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
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b == a,
                .FLOAT64 => |b| @as(f64, @floatFromInt(a)) == b,
                else => false,
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| a == @as(f64, @floatFromInt(b)),
                .FLOAT64 => |b| a == b,
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
                .ARRAY => false,
                .FUNCTION_EXPR => false,
            },
            .NIL => switch (RB) {
                .NIL => true,
                else => false,
            },
            .NATIVEF => false,
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
    }

    inline fn cmpNotEqual(self: *VM, instruction: Instruction) void {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b != a,
                .FLOAT64 => |b| @as(f64, @floatFromInt(a)) != b,
                else => false,
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| a != @as(f64, @floatFromInt(b)),
                .FLOAT64 => |b| a != b,
                else => false,
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
                .ARRAY => false,
                .FUNCTION_EXPR => false,
            },
            .NIL => switch (RB) {
                .NIL => false,
                else => true,
            },
            .NATIVEF => false,
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
    }

    inline fn cmpLessThan(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b < a,
                .FLOAT64 => |b| b < @as(f64, @floatFromInt(a)),
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| @as(f64, @floatFromInt(b)) < a,
                .FLOAT64 => |b| b < a,
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            else => {
                self.rError("TypeError: left operand must be numeric, got {s}", .{@tagName(RA)});
                return false;
            },
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
        return true;
    }

    inline fn cmpGreaterThan(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b > a,
                .FLOAT64 => |b| b > @as(f64, @floatFromInt(a)),
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| @as(f64, @floatFromInt(b)) > a,
                .FLOAT64 => |b| b > a,
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            else => {
                self.rError("TypeError: left operand must be numeric, got {s}", .{@tagName(RA)});
                return false;
            },
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
        return true;
    }

    inline fn cmpLessEqual(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b <= a,
                .FLOAT64 => |b| b <= @as(f64, @floatFromInt(a)),
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| @as(f64, @floatFromInt(b)) <= a,
                .FLOAT64 => |b| b <= a,
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            else => {
                self.rError("TypeError: left operand must be numeric, got {s}", .{@tagName(RA)});
                return false;
            },
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
        return true;
    }

    inline fn cmpGreaterEqual(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        const result = switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| b >= a,
                .FLOAT64 => |b| b >= @as(f64, @floatFromInt(a)),
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| @as(f64, @floatFromInt(b)) >= a,
                .FLOAT64 => |b| b >= a,
                else => {
                    self.rError("TypeError: right operand must be numeric, got {s}", .{@tagName(RB)});
                    return false;
                },
            },
            else => {
                self.rError("TypeError: left operand must be numeric, got {s}", .{@tagName(RA)});
                return false;
            },
        };

        self.currentCallFrameRegisters().set(RC, Value.createBoolean(result));
        return true;
    }

    inline fn mathAdd(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createI64(b + a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b + @as(f64, @floatFromInt(a))));
                },
                .OBJECT => |o| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(o.data), @tagName(RA) });
                    return false;
                },
                else => |p| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(p), @tagName(RA) });
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(@as(f64, @floatFromInt(b)) + a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b + a));
                },
                .OBJECT => |o| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(o.data), @tagName(RA) });
                    return false;
                },
                else => |p| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(p), @tagName(RA) });
                    return false;
                },
            },
            .OBJECT => |a| switch (a.data) {
                .STRING => |str_a| {
                    if (RB.asZigString()) |str_b| {
                        const result = std.heap.page_allocator.alloc(u8, str_b.len + str_a.chars.len) catch unreachable;
                        @memcpy(result[0..str_b.len], str_b);
                        @memcpy(result[str_b.len..], str_a.chars);

                        const value = Value.kidnapString(std.heap.page_allocator, result, self.strings, &self.objects);

                        self.currentCallFrameRegisters().set(RC, value);
                    } else {
                        self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(RB), @tagName(a.data) });
                        return false;
                    }
                },
                else => |p| {
                    self.rError("type error: operands must be both numeric or string, got {s} and {s}", .{ @tagName(p), @tagName(RA) });
                    return false;
                },
            },
            else => |p| {
                self.rError("type error: operands must be both numeric or string, got {s}", .{@tagName(p)});
                return false;
            },
        }

        return true;
    }

    inline fn mathSub(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createI64(b - a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b - @as(f64, @floatFromInt(a))));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(@as(f64, @floatFromInt(b)) - a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b - a));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                return false;
            },
        }

        return true;
    }

    inline fn mathMul(self: *VM, instruction: Instruction) bool { // add string multiplication
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createI64(b * a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b * @as(f64, @floatFromInt(a))));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(@as(f64, @floatFromInt(b)) * a));
                },
                .FLOAT64 => |b| {
                    self.currentCallFrameRegisters().set(RC, Value.createF64(b * a));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                return false;
            },
        }

        return true;
    }

    inline fn mathDiv(self: *VM, instruction: Instruction) bool {
        const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(instruction));
        const RB = self.currentCallFrameRegisters().get(_instruction.DECODE_RB(instruction));
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.currentCallFrameRegisters().set(RC, Value{ .INT64 = @divTrunc(b, a) });
                    }
                },
                .FLOAT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.currentCallFrameRegisters().set(RC, Value.createF64(b / @as(f64, @floatFromInt(a))));
                    }
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.currentCallFrameRegisters().set(RC, Value.createF64(@as(f64, @floatFromInt(b)) / a));
                    }
                },
                .FLOAT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.currentCallFrameRegisters().set(RC, Value.createF64(b / a));
                    }
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            else => |p| {
                self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                return false;
            },
        }

        return true;
    }

    pub fn run(self: *VM) bool {
        @setRuntimeSafety(false);

        var frame: *CallFrame = &self.frames.slice()[self.frameIndex];
        var pc = frame.pc;

        while (true) {
            const curInstruction = frame.function.instructions.items[pc];
            pc += 1;
            frame.pc = pc;

            const opcode = _instruction.GET_OPCODE(curInstruction);
            //std.debug.print("OP {s} @ PC={d}\n", .{ @tagName(opcode), pc });
            switch (opcode) {
                .LOADK => {
                    const constantIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const contantValue = self.GET_CONSTANT(constantIdx).?;

                    self.currentCallFrameRegisters().set(RC, contantValue);
                },
                .LOADF => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.currentCallFrameRegisters().set(RC, Value.createBoolean(false));
                },
                .LOADT => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.currentCallFrameRegisters().set(RC, Value.createBoolean(true));
                },
                .LOADN => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.currentCallFrameRegisters().set(RC, Value.createNil());
                },
                .ADD => {
                    if (!self.mathAdd(curInstruction)) return false;
                },
                .SUB => {
                    if (!self.mathSub(curInstruction)) return false;
                },
                .MUL => {
                    if (!self.mathMul(curInstruction)) return false;
                },
                .DIV => {
                    if (!self.mathDiv(curInstruction)) return false;
                },
                .EQUAL => {
                    self.cmpEqual(curInstruction); // ==
                },
                .NOTEQUAL => {
                    self.cmpNotEqual(curInstruction); // !=
                },
                .LESSEQUAL => {
                    if (!self.cmpLessEqual(curInstruction)) return false; // <=
                },
                .GREATEREQUAL => {
                    if (!self.cmpGreaterEqual(curInstruction)) return false; // >=
                },
                .GREATERTHAN => {
                    if (!self.cmpGreaterThan(curInstruction)) return false; // >
                },
                .LESSTHAN => {
                    if (!self.cmpLessThan(curInstruction)) return false; // <
                },
                .BANG => {
                    const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    switch (RA) {
                        .BOOLEAN => |n| self.currentCallFrameRegisters().set(RC, Value.createBoolean(!n)),
                        else => |p| {
                            self.rError("type error: operand must be boolean, got {s}", .{@tagName(p)});
                            return false;
                        },
                    }
                },
                .MINUS => {
                    const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    switch (RA) {
                        .INT64 => |n| self.currentCallFrameRegisters().set(RC, Value.createI64(-n)),
                        .FLOAT64 => |n| self.currentCallFrameRegisters().set(RC, Value.createF64(-n)),
                        else => |p| {
                            self.rError("type error: operand must be numeric, got {s}", .{@tagName(p)});
                            return false;
                        },
                    }
                },
                .JMPF => {
                    const RC = self.currentCallFrameRegisters().get(_instruction.DECODE_RC(curInstruction));

                    if (!RC.isTruthy()) {
                        const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);
                        pc += jumpOffset;
                    }
                },
                .JMP => {
                    const jumpOffset = _instruction.DECODE_JUMP_OFFSET(curInstruction);
                    pc += jumpOffset;
                },
                .SGLOBAL => {
                    const RC = self.currentCallFrameRegisters().get(_instruction.DECODE_RC(curInstruction));
                    const globalIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);

                    self.globals.slice()[globalIdx] = RC;
                },
                .GGLOBAL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const globalIdx = _instruction.DECODE_CONSTANT_IDX(curInstruction);

                    self.currentCallFrameRegisters().set(RC, self.globals.slice()[globalIdx]);
                },
                .PUSH => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    self.push(self.currentCallFrameRegisters().get(RC));
                    //                     std.debug.print("pushed: {d}\n", .{self.currentCallFrameRegisters().get(RC).NUMBER});
                },
                .BCALL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(curInstruction));
                    const RB = _instruction.DECODE_RB(curInstruction);

                    const args_slice = self.stack.slice()[self.stack.len - RB .. self.stack.len];
                    _value.printStdOut("before\n", .{});

                    _value.printStdOut("{any}\n", .{args_slice});

                    _value.printStdOut("after\n", .{});

                    switch (RA.NATIVEF.function) {
                        .arity1 => |nfn| {
                            if (RB != 1) {
                                self.rError("argument error: expected {d} arguments but got {d}", .{ 1, RB });
                                return false;
                            }

                            const result: Value = nfn(args_slice[0]);

                            frame.registers.set(RC, result);
                        },
                        else => unreachable,
                    }

                    self.stack.len -= RB;
                },
                .CALL => {
                    const RC = _instruction.DECODE_RC(curInstruction);
                    const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(curInstruction));
                    const RB = _instruction.DECODE_RB(curInstruction);
                    const f = RA.asFunctionExpr();

                    if (f == null) {
                        self.rError("type error: tried calling non-function: {any}", .{@tagName(RA)});
                        return false;
                    }

                    var callFrame = CallFrame.init(f.?, RC);

                    if (f.?.params_registers != null and f.?.params_registers.?.len != RB) {
                        self.rError("argument error: expected {d} arguments but got {d}", .{ f.?.params_registers.?.len, RB });
                        return false;
                    }

                    const stack_start = self.stack.len - RB;
                    for (0..RB) |i| {
                        callFrame.registers.set(i + 1, self.stack.slice()[stack_start + i]);
                    }
                    self.stack.len -= RB;

                    self.pushCallFrame(callFrame);

                    frame = self.currentCallFrame();
                    frame.registers.set(0, self.currentCallFrameRegisters().get(0));

                    pc = 0;
                },
                .RET => {
                    const RC = self.currentCallFrameRegisters().get(_instruction.DECODE_RC(curInstruction));

                    const poppedFrame = self.popCallFrame();

                    if (self.frameIndex == 0) return true; // if we are in frame 0, the program has finished executing

                    frame = self.currentCallFrame();
                    self.currentCallFrameRegisters().set(poppedFrame.return_register, RC);
                    pc = frame.pc;
                },
                .RETN => {
                    const poppedFrame = self.popCallFrame();

                    if (self.frameIndex == 0) return true; // if we are in frame 0, the program has finished executing

                    frame = self.currentCallFrame();

                    self.currentCallFrameRegisters().set(poppedFrame.return_register, NIL);
                    pc = frame.pc;
                },
                .MOVE => {
                    const RA = self.currentCallFrameRegisters().get(_instruction.DECODE_RA(curInstruction));
                    const RC = _instruction.DECODE_RC(curInstruction);

                    self.currentCallFrameRegisters().set(RC, RA);
                },
                else => {
                    self.rError("Unhandled OPCODE: {any} \n", .{opcode});
                    return false;
                },
            }
        }

        return true;
    }
};

const std = @import("std");
const expect = std.testing.expect;
const mem = std.mem;
const is_debug = @import("builtin").mode == .Debug;

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
pub const MAX_GLOBALS = 1024;
//pub const MAX_GLOBALS = 65535;
pub const MAX_FRAMES = 128;
pub const STACK_SIZE = 1024;

pub const NIL = Value{ .NIL = void{} };
pub const TRUE = Value{ .BOOLEAN = true };
pub const FALSE = Value{ .BOOLEAN = false };

const CallFrame = struct {
    function: *FunctionExpr,
    pc: usize,
    return_register: u8,
    register_offset: usize,

    pub fn init(function: *FunctionExpr, return_register: u8, register_offset: usize) CallFrame {
        return CallFrame{
            .pc = 0,
            .function = function,
            .return_register = return_register,
            .register_offset = register_offset,
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

    cregisters: []Value,
    wregisters: [MAX_FRAMES * MAX_REGISTERS]Value,

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

        @memset(&vm.wregisters, NIL);
        vm.cregisters = vm.wregisters[0..MAX_REGISTERS];

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
        vm.frames.set(1, CallFrame.init(mainFunction.asFunctionExpr().?, 0, 0));

        for (_nativef.native_functions, 0..) |_, i| {
            vm.defineNative(@as(u16, @intCast(i)));
        }

        return vm;
    }

    pub fn repl_init(allocator: std.mem.Allocator, constantsPool: *std.ArrayList(Value), compiledInst: *CompilationScope, globals: *std.BoundedArray(Value, MAX_GLOBALS), source: *[]const u8, strings: *std.StringHashMap(Value), objects: ?*Object) *VM {
        const vm = allocator.create(VM) catch unreachable;

        @memset(&vm.wregisters, NIL);
        vm.cregisters = vm.wregisters[0..MAX_REGISTERS];

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
        vm.frames.set(1, CallFrame.init(mainFunction.asFunctionExpr().?, 0, 0));

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

    inline fn currentCallFrameRegisters(self: *VM) *[]Value {
        return &self.cregisters;
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
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
    }

    inline fn cmpNotEqual(self: *VM, instruction: Instruction) void {
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
    }

    inline fn cmpLessThan(self: *VM, instruction: Instruction) bool {
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
        return true;
    }

    inline fn cmpGreaterThan(self: *VM, instruction: Instruction) bool {
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
        return true;
    }

    inline fn cmpLessEqual(self: *VM, instruction: Instruction) bool {
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
        return true;
    }

    inline fn cmpGreaterEqual(self: *VM, instruction: Instruction) bool {
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
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

        self.cregisters[RC] = Value.createBoolean(result);
        return true;
    }

    inline fn mathAdd(self: *VM, instruction: Instruction) bool {
        const RA = self.cregisters[(_instruction.DECODE_RA(instruction))];
        const RB = self.cregisters[(_instruction.DECODE_RB(instruction))];
        const RC = _instruction.DECODE_RC(instruction);

        if (RA.isInt() and RB.isInt()) {
            self.cregisters[RC] = Value{ .INT64 = RB.INT64 + RA.INT64 };
            return true;
        }

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b + @as(f64, @floatFromInt(a)));
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
                    self.cregisters[RC] = Value.createF64(@as(f64, @floatFromInt(b)) + a);
                },
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b + a);
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

                        self.cregisters[RC] = value;
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
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.cregisters[RC] = Value.createI64(b - a);
                },
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b - @as(f64, @floatFromInt(a)));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.cregisters[RC] = Value.createF64(@as(f64, @floatFromInt(b)) - a);
                },
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b - a);
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
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.cregisters[RC] = Value.createI64(b * a);
                },
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b * @as(f64, @floatFromInt(a)));
                },
                else => |p| {
                    self.rError("type error: operands must be numeric, got {s}", .{@tagName(p)});
                    return false;
                },
            },
            .FLOAT64 => |a| switch (RB) {
                .INT64 => |b| {
                    self.cregisters[RC] = Value.createF64(@as(f64, @floatFromInt(b)) * a);
                },
                .FLOAT64 => |b| {
                    self.cregisters[RC] = Value.createF64(b * a);
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
        const RA = self.cregisters[_instruction.DECODE_RA(instruction)];
        const RB = self.cregisters[_instruction.DECODE_RB(instruction)];
        const RC = _instruction.DECODE_RC(instruction);

        switch (RA) {
            .INT64 => |a| switch (RB) {
                .INT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.cregisters[RC] = Value{ .INT64 = @divTrunc(b, a) };
                    }
                },
                .FLOAT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.cregisters[RC] = Value.createF64(b / @as(f64, @floatFromInt(a)));
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
                        self.cregisters[RC] = Value.createF64(@as(f64, @floatFromInt(b)) / a);
                    }
                },
                .FLOAT64 => |b| {
                    if (a == 0) {
                        self.rError("numeric error: division by zero", .{});
                    } else {
                        self.cregisters[RC] = Value.createF64(b / a);
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

        fetch: switch (_instruction.GET_OPCODE(frame.function.instructions.items[frame.pc])) {
            .LOADK => {
                const instr = frame.function.instructions.items[frame.pc];

                const constantIdx = _instruction.DECODE_CONSTANT_IDX(instr);
                const RC = _instruction.DECODE_RC(instr);
                const contantValue = self.GET_CONSTANT(constantIdx).?;

                self.cregisters[RC] = contantValue;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .LOADF => {
                const RC = _instruction.DECODE_RC(frame.function.instructions.items[frame.pc]);
                self.cregisters[RC] = Value.createBoolean(false);

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .LOADT => {
                const RC = _instruction.DECODE_RC(frame.function.instructions.items[frame.pc]);
                self.cregisters[RC] = Value.createBoolean(true);

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .LOADN => {
                const RC = _instruction.DECODE_RC(frame.function.instructions.items[frame.pc]);
                self.cregisters[RC] = Value.createNil();

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .ADD => {
                if (!self.mathAdd(frame.function.instructions.items[frame.pc])) return false;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .SUB => {
                if (!self.mathSub(frame.function.instructions.items[frame.pc])) return false;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .MUL => {
                if (!self.mathMul(frame.function.instructions.items[frame.pc])) return false;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .DIV => {
                if (!self.mathDiv(frame.function.instructions.items[frame.pc])) return false;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .EQUAL => {
                self.cmpEqual(frame.function.instructions.items[frame.pc]); // ==

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .NOTEQUAL => {
                self.cmpNotEqual(frame.function.instructions.items[frame.pc]); // !=

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .LESSEQUAL => {
                if (!self.cmpLessEqual(frame.function.instructions.items[frame.pc])) return false; // <=

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .GREATEREQUAL => {
                if (!self.cmpGreaterEqual(frame.function.instructions.items[frame.pc])) return false; // >=

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .GREATERTHAN => {
                if (!self.cmpGreaterThan(frame.function.instructions.items[frame.pc])) return false; // >

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .LESSTHAN => {
                if (!self.cmpLessThan(frame.function.instructions.items[frame.pc])) return false; // <

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .BANG => {
                const instr = frame.function.instructions.items[frame.pc];

                const RA = self.cregisters[_instruction.DECODE_RA(instr)];
                const RC = _instruction.DECODE_RC(instr);

                switch (RA) {
                    .BOOLEAN => |n| self.cregisters[RC] = Value.createBoolean(!n),
                    else => |p| {
                        self.rError("type error: operand must be boolean, got {s}", .{@tagName(p)});
                        return false;
                    },
                }

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .MINUS => {
                const instr = frame.function.instructions.items[frame.pc];
                const RA = self.cregisters[_instruction.DECODE_RA(instr)];
                const RC = _instruction.DECODE_RC(instr);

                switch (RA) {
                    .INT64 => |n| self.cregisters[RC] = Value.createI64(-n),
                    .FLOAT64 => |n| self.cregisters[RC] = Value.createF64(-n),
                    else => |p| {
                        self.rError("type error: operand must be numeric, got {s}", .{@tagName(p)});
                        return false;
                    },
                }

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .JMPF => {
                const instr = frame.function.instructions.items[frame.pc];
                const RC = self.cregisters[_instruction.DECODE_RC(instr)];

                if (!RC.isTruthy()) {
                    const jumpOffset = _instruction.DECODE_JUMP_OFFSET(instr);
                    frame.pc += jumpOffset;
                }

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .JMP => {
                const instr = frame.function.instructions.items[frame.pc];

                const jumpOffset = _instruction.DECODE_JUMP_OFFSET(instr);
                frame.pc += jumpOffset + 1;

                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .SGLOBAL => {
                const instr = frame.function.instructions.items[frame.pc];

                const RC = self.cregisters[_instruction.DECODE_RC(instr)];
                const globalIdx = _instruction.DECODE_CONSTANT_IDX(instr);

                self.globals.slice()[globalIdx] = RC;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .GGLOBAL => {
                const instr = frame.function.instructions.items[frame.pc];

                const RC = _instruction.DECODE_RC(instr);
                const globalIdx = _instruction.DECODE_CONSTANT_IDX(instr);

                self.cregisters[RC] = self.globals.slice()[globalIdx];

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .PUSH => {
                const RC = _instruction.DECODE_RC(frame.function.instructions.items[frame.pc]);
                self.push(self.cregisters[RC]);

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .BCALL => {
                const instr = frame.function.instructions.items[frame.pc];

                //const RC = _instruction.DECODE_RC(instr);
                const RA = self.cregisters[_instruction.DECODE_RA(instr)];
                const RB = _instruction.DECODE_RB(instr);

                const args_slice = self.stack.slice()[self.stack.len - RB .. self.stack.len];

                switch (RA.NATIVEF.function) {
                    .arity1 => |nfn| {
                        if (RB != 1) {
                            self.rError("argument error: expected {d} arguments but got {d}", .{ 1, RB });
                            return false;
                        }

                        const result: Value = nfn(args_slice[0]);

                        self.cregisters[0] = result;
                    },
                    else => unreachable,
                }

                self.stack.len -= RB;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .CALL => {
                const instr = frame.function.instructions.items[frame.pc];

                const RC = _instruction.DECODE_RC(instr);
                const RA = self.cregisters[_instruction.DECODE_RA(instr)];
                const RB = _instruction.DECODE_RB(instr);
                const f = RA.asFunctionExpr() orelse {
                    self.rError("type error: tried calling non-function: {any}", .{@tagName(RA)});
                    return false;
                };

                const new_offset = frame.register_offset + MAX_REGISTERS;

                //                 if (f.?.params_registers != null and f.?.params_registers.?.len != RB) {
                //                     self.rError("argument error: expected {d} arguments but got {d}", .{ f.?.params_registers.?.len, RB });
                //                     return false;
                //                 }
                self.cregisters = self.wregisters[new_offset .. new_offset + MAX_REGISTERS];

                const args_start = self.stack.len - RB;
                for (0..RB) |i| {
                    self.wregisters[new_offset + i + 1] = self.stack.buffer[args_start + i];
                }
                self.stack.len -= RB;

                frame.pc += 1;

                self.pushCallFrame(CallFrame.init(f, RC, new_offset));
                frame = self.currentCallFrame();

                frame.pc = 0;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .RET => {
                const RC = self.cregisters[_instruction.DECODE_RC(frame.function.instructions.items[frame.pc])];

                const popped_frame = self.popCallFrame();

                if (self.frameIndex == 0) return true; // if we are in frame 0, the program has finished executing

                frame = self.currentCallFrame();

                const prev_offset = if (self.frameIndex == 0)
                    0
                else
                    self.frames.slice()[self.frameIndex].register_offset;

                self.cregisters = self.wregisters[prev_offset .. prev_offset + MAX_REGISTERS];

                self.cregisters[popped_frame.return_register] = RC;

                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .RETN => {
                const popped_frame = self.popCallFrame();

                if (self.frameIndex == 0) return true; // if we are in frame 0, the program has finished executing

                const prev_offset = if (self.frameIndex == 0)
                    0
                else
                    self.frames.slice()[self.frameIndex].register_offset;

                frame = self.currentCallFrame();

                self.cregisters = self.wregisters[prev_offset .. prev_offset + MAX_REGISTERS];
                self.cregisters[popped_frame.return_register] = NIL;

                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            .MOVE => {
                const instr = frame.function.instructions.items[frame.pc];

                const RA = self.cregisters[_instruction.DECODE_RA(instr)];
                const RC = _instruction.DECODE_RC(instr);

                self.cregisters[RC] = RA;

                frame.pc += 1;
                continue :fetch _instruction.GET_OPCODE(frame.function.instructions.items[frame.pc]);
            },
            else => {
                self.rError("Unhandled OPCODE: {any} \n", .{_instruction.GET_OPCODE(frame.function.instructions.items[frame.pc])});
                return false;
            },
        }

        return true;
    }
};

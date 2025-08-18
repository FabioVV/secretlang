const std = @import("std");
const VM = @import("vm.zig").VM;
const _nativef = @import("nativef.zig");
const CompilationScope = @import("compiler.zig").CompilationScope;
const Instruction = @import("instruction.zig").Instruction;
const Position = @import("token.zig").Position;

pub fn printStdOut(comptime str: []const u8, varagars: anytype) void {
    _ = std.io.getStdOut().writer().print(str, varagars) catch unreachable;
}

pub const ValueType = enum {
    INT64,
    FLOAT64,
    BOOLEAN,
    NIL,
    OBJECT,
    NATIVEF,
};

pub const ObjectTypes = enum {
    STRING,
    ARRAY,
    FUNCTION_EXPR,
};

pub const Array = struct {
    items: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) *Array {
        const arr = allocator.create(Array) catch unreachable;

        arr.items = std.ArrayList(Value).init(allocator) catch unreachable;

        return arr;
    }
};

pub const String = struct {
    chars: []const u8,

    pub fn init(allocator: std.mem.Allocator, str: []const u8) *String {
        const string_obj = allocator.create(String) catch unreachable;
        const owned_chars = allocator.dupe(u8, str) catch unreachable;

        string_obj.* = String{ .chars = owned_chars };
        return string_obj;
    }
};

pub const FunctionExpr = struct {
    instructions: std.ArrayList(Instruction),
    instructions_positions: std.AutoHashMap(u32, Position),
    params_registers: ?std.BoundedArray(u8, 32),

    pub fn init(allocator: std.mem.Allocator, compiledFn: CompilationScope, params_registers: ?std.BoundedArray(u8, 32)) *FunctionExpr {
        const fn_obj = allocator.create(FunctionExpr) catch unreachable;

        var compiledFunction = compiledFn;
        const compiledInstructions = compiledFunction.instructions.toOwnedSlice() catch unreachable;

        var instructions = std.ArrayList(Instruction).init(allocator);
        instructions.appendSlice(compiledInstructions) catch unreachable;

        fn_obj.* = FunctionExpr{
            .instructions = instructions,
            .instructions_positions = compiledFn.instructions_positions.cloneWithAllocator(allocator) catch unreachable,
            .params_registers = params_registers,
        };

        compiledFunction.instructions.deinit();
        compiledFunction.instructions_positions.deinit();

        return fn_obj;
    }
};

pub const Object = struct {
    next: ?*Object,

    data: union(ObjectTypes) {
        STRING: *String,
        ARRAY: *Array,
        FUNCTION_EXPR: *FunctionExpr,
    },

    pub fn initFnExpr(allocator: std.mem.Allocator, data: *FunctionExpr, objects: *?*Object) *Object {
        const obj = allocator.create(Object) catch unreachable;

        obj.* = Object{
            .data = .{ .FUNCTION_EXPR = data },
            .next = objects.*,
        };

        objects.* = obj;

        return obj;
    }

    pub fn initString(allocator: std.mem.Allocator, data: *String, objects: *?*Object) *Object {
        const obj = allocator.create(Object) catch unreachable;

        obj.* = Object{
            .data = .{ .STRING = data },
            .next = objects.*,
        };

        objects.* = obj;

        return obj;
    }

    pub fn initArray(allocator: std.mem.Allocator, data: *Array, objects: *?*Object) *Object {
        const obj = allocator.create(Object) catch unreachable;

        obj.* = Object{
            .data = .{ .ARRAY = data },
            .next = objects.*,
        };

        objects.* = obj;

        return obj;
    }
};

pub const Value = union(ValueType) {
    INT64: i64,
    FLOAT64: f64,
    BOOLEAN: bool,
    NIL: void,
    OBJECT: *Object,
    NATIVEF: *_nativef.nativeFunction,

    pub inline fn hash(self: Value) u64 {
        var hasher = std.hash.Wyhash.init(0);

        switch (self) {
            .INT64 => |n| {
                hasher.update(&std.mem.toBytes(n));
            },
            .FLOAT64 => |n| {
                const bits = @as(u64, @bitCast(n));
                hasher.update(&std.mem.toBytes(bits));
            },
            .BOOLEAN => |b| hasher.update(&std.mem.toBytes(b)),
            .NIL => |n| hasher.update(&std.mem.toBytes(n)),
            .OBJECT => |o| switch (o.data) {
                .STRING => |s| hasher.update(&std.mem.toBytes(s.chars)),
                .ARRAY => |a| {
                    hasher.update(&std.mem.toBytes(a.*));
                },
                .FUNCTION_EXPR => |f| {
                    hasher.update(&std.mem.toBytes(f.*));
                },
            },
            .NATIVEF => |f| {
                hasher.update(&std.mem.toBytes(f.*));
            },
        }

        return hasher.final();
    }

    pub inline fn createF64(num: f64) Value {
        return Value{ .FLOAT64 = num };
    }

    pub inline fn createI64(num: i64) Value {
        return Value{ .INT64 = num };
    }

    pub inline fn createNil() Value {
        return Value{ .NIL = void{} };
    }

    pub inline fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub inline fn createFunctionExpr(allocator: std.mem.Allocator, compiledFn: CompilationScope, params_registers: ?std.BoundedArray(u8, 32), objects: *?*Object) Value {
        const fn_obj = FunctionExpr.init(allocator, compiledFn, params_registers);
        const obj = Object.initFnExpr(allocator, fn_obj, objects);

        const val = Value{ .OBJECT = obj };

        return val;
    }

    pub inline fn getNative(gindex: u16) Value {
        return Value{
            .NATIVEF = @constCast(&_nativef.native_functions[gindex]),
        };
    }

    pub inline fn createArray(allocator: std.mem.Allocator, objects: *?*Object) Value {
        const array_obj = Array.init(allocator);
        const obj = Object.initArray(allocator, array_obj, objects);

        const val = Value{ .OBJECT = obj };

        return val;
    }

    pub fn allocString(allocator: std.mem.Allocator, str: []const u8, string_table: *std.StringHashMap(Value), objects: *?*Object) Value {
        if (string_table.get(str)) |our_str| {
            return our_str;
        }

        const string_obj = String.init(allocator, str);
        const obj = Object.initString(allocator, string_obj, objects);
        const val = Value{ .OBJECT = obj };

        string_table.put(allocator.dupe(u8, str) catch unreachable, val) catch unreachable;

        return val;
    }

    pub fn kidnapString(allocator: std.mem.Allocator, str: []const u8, string_table: *std.StringHashMap(Value), objects: *?*Object) Value {
        if (string_table.get(str)) |our_str| {
            allocator.free(str);
            return our_str;
        }

        return allocString(allocator, str, string_table, objects);
    }

    pub fn copyString(allocator: std.mem.Allocator, str: []const u8, string_table: *std.StringHashMap(Value), objects: *?*Object) Value {
        return allocString(allocator, str, string_table, objects);
    }

    pub inline fn asZigString(self: Value) ?[]const u8 {
        return switch (self) {
            .OBJECT => |obj| switch (obj.*.data) {
                .STRING => |str| str.chars,
                else => null,
            },
            else => null,
        };
    }

    pub inline fn asObject(self: Value) ?*Object {
        return switch (self) {
            .OBJECT => |obj| obj,
            else => null,
        };
    }

    pub inline fn asZigArray(self: Value) ?*Array {
        return switch (self) {
            .OBJECT => |obj| switch (obj.*.data) {
                .ARRAY => |arr| arr,
                else => null,
            },
            else => null,
        };
    }

    pub inline fn asFunctionExpr(self: Value) ?*FunctionExpr {
        return switch (self) {
            .OBJECT => |obj| switch (obj.*.data) {
                .FUNCTION_EXPR => |f| f,
                else => null,
            },
            else => null,
        };
    }

    pub inline fn asNativeFunction(self: Value) ?*_nativef.nativeFunction {
        return switch (self) {
            .NATIVEF => |nfn| nfn,
            else => null,
        };
    }

    pub inline fn getType(self: Value) ValueType {
        return switch (self) {
            .INT64 => .INT64,
            .FLOAT64 => .FLOAT64,
            .BOOLEAN => .BOOLEAN,
            .NIL => .NIL,
            .OBJECT => .OBJECT,
        };
    }

    pub inline fn isNumber(self: Value) bool {
        return self == .FLOAT64 or self == .INT64;
    }

    pub inline fn isInt(self: Value) bool {
        return self == .INT64;
    }

    pub inline fn isFloat(self: Value) bool {
        return self == .FLOAT64;
    }

    pub inline fn isBoolean(self: Value) bool {
        return self == .BOOLEAN;
    }

    pub inline fn isString(self: Value) bool {
        return switch (self) {
            .OBJECT => |obj| switch (obj.data) {
                .STRING => true,
                else => false,
            },
            else => false,
        };
    }

    pub inline fn isNil(self: Value) bool {
        return self == .NIL;
    }

    pub inline fn isObject(self: Value) bool {
        return self == .OBJECT;
    }

    pub inline fn isTruthy(self: Value) bool {
        return switch (self) {
            .BOOLEAN => |b| b,
            .NIL => false,
            .INT64 => |n| n != 0,
            .FLOAT64 => |n| n != 0.0,
            .OBJECT => |obj| switch (obj.data) {
                .STRING => |str| str.chars.len > 0,
                .ARRAY => |arr| arr.items.items.len > 0,
                .FUNCTION_EXPR => true,
            },
            .NATIVEF => true,
        };
    }

    pub fn printArrItems(arr: *Array) void {
        printStdOut("[", .{});
        for (arr.items.items) |arr_i| {
            switch (arr_i) {
                .BOOLEAN => |b| printStdOut("{}, ", .{b}),
                .NIL => printStdOut("nil, ", .{}),
                .FLOAT64 => |n| printStdOut("{d}, ", .{n}),
                .INT64 => |n| printStdOut("{d}, ", .{n}),
                .OBJECT => |inn_obj| switch (inn_obj.*.data) {
                    .STRING => printStdOut("{s}, ", .{inn_obj.data.STRING.chars}),
                    .ARRAY => printArrItems(inn_obj.data.ARRAY),
                    .FUNCTION_EXPR => printStdOut("<anonymous function expression at>, ", .{}),
                },
                .NATIVEF => |native_fn| {
                    const addr = switch (native_fn.function) {
                        .arity1 => |f| @intFromPtr(f),
                        else => unreachable,
                    };
                    printStdOut("<native function '{s}' at 0x{X}>, ", .{ native_fn.name, addr });
                },
            }
        }
        printStdOut("]", .{});
    }

    pub inline fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| printStdOut("{}\n", .{b}),
            .NIL => printStdOut("nil\n", .{}),
            .FLOAT64 => |n| printStdOut("{d}\n", .{n}),
            .INT64 => |n| printStdOut("{d}\n", .{n}),
            .OBJECT => |obj| switch (obj.*.data) {
                .STRING => printStdOut("{s}\n", .{obj.data.STRING.chars}),
                .ARRAY => printArrItems(obj.data.ARRAY),
                .FUNCTION_EXPR => printStdOut("<anonymous function expression at>\n", .{}),
            },
            .NATIVEF => |native_fn| {
                const addr = switch (native_fn.function) {
                    .arity1 => |f| @intFromPtr(f),
                    else => unreachable,
                };
                printStdOut("<native function '{s}' at 0x{X}>\n", .{ native_fn.name, addr });
            },
        };
    }
};

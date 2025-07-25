const std = @import("std");
const VM = @import("vm.zig").VM;

pub const ValueType = enum {
    NUMBER,
    BOOLEAN,
    NIL,
    OBJECT,
};

pub const ObjectTypes = enum {
    STRING,
    ARRAY,
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

pub const Object = struct {
    next: ?*Object,

    data: union(ObjectTypes) {
        STRING: *String,
        ARRAY: *Array,
    },

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
    NUMBER: f64,
    BOOLEAN: bool,
    NIL: void,
    OBJECT: *Object,

    pub inline fn hash(self: Value) u64 {
        var hasher = std.hash.Wyhash.init(0);

        switch (self) {
            .NUMBER => |n| {
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
            },
        }

        return hasher.final();
    }

    pub inline fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub inline fn createNil() Value {
        return Value{ .NIL = void{} };
    }

    pub inline fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub inline fn createArray(allocator: std.mem.Allocator, objects: *?*Object) Value {
        const array_obj = Array.init(allocator);
        const obj = Object.initArray(allocator, array_obj, objects);

        const val = Value{ .OBJECT = obj };

        return val;
    }

    pub inline fn allocString(allocator: std.mem.Allocator, str: []const u8, string_table: *std.StringHashMap(Value), objects: *?*Object) Value {
        const string_obj = String.init(allocator, str);
        const obj = Object.initString(allocator, string_obj, objects);
        const val = Value{ .OBJECT = obj };

        string_table.put(string_obj.chars, val) catch unreachable;

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
        if (string_table.get(str)) |our_str| {
            return our_str;
        }

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

    pub inline fn asZigArray(self: Value) ?*Array {
        return switch (self) {
            .OBJECT => |obj| switch (obj.*.data) {
                .ARRAY => |arr| arr,
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

    pub inline fn isNumber(self: Value) bool {
        return self == .NUMBER;
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
            .NUMBER => |n| n != 0,
            .OBJECT => |obj| switch (obj.data) {
                .STRING => |str| str.chars.len > 0,
                .ARRAY => |arr| arr.items.items.len > 0,
            },
        };
    }

    pub fn printArrItems(arr: *Array) void {
        std.debug.print("[", .{});
        for (arr.items.items) |arr_i| {
            switch (arr_i) {
                .BOOLEAN => |b| std.debug.print("{},", .{b}),
                .NIL => std.debug.print("nil,", .{}),
                .NUMBER => |n| std.debug.print("{d:.2},", .{n}),
                .OBJECT => |inn_obj| switch (inn_obj.*.data) {
                    .STRING => std.debug.print("{s},", .{inn_obj.data.STRING.chars}),
                    .ARRAY => printArrItems(inn_obj.data.ARRAY),
                },
            }
        }
        std.debug.print("]", .{});
    }

    pub inline fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| std.debug.print("{}\n", .{b}),
            .NIL => std.debug.print("nil\n", .{}),
            .NUMBER => |n| std.debug.print("{d:.2}\n", .{n}),
            .OBJECT => |obj| switch (obj.*.data) {
                .STRING => std.debug.print("{s}\n", .{obj.data.STRING.chars}),
                .ARRAY => printArrItems(obj.data.ARRAY),
            },
        };
    }
};

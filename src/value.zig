const std = @import("std");

pub const ValueType = enum {
    NUMBER,
    BOOLEAN,
    NIL,
    OBJECT,
};

pub const ObjectTypes = enum {
    STRING,
};

pub const String = struct {
    chars: []const u8,

    pub fn create(allocator: std.mem.Allocator, str: []const u8) *String {
        const string_obj = allocator.create(String) catch unreachable;
        string_obj.* = String{ .chars = str };
        return string_obj;
    }
};

pub const Object = union(ObjectTypes) {
    STRING: *String,
};

pub const Value = union(ValueType) {
    NUMBER: f64,
    BOOLEAN: bool,
    NIL: void,
    OBJECT: *Object,

    pub inline fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub inline fn createNil() Value {
        return Value{ .NIL = void{} };
    }

    pub inline fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub inline fn createString(allocator: std.mem.Allocator, str: []const u8) Value {
        const string_obj = String.create(allocator, str);
        const obj =  allocator.create(Object) catch unreachable;
        obj.* = Object{ .STRING = string_obj };
        return Value{ .OBJECT = obj };
    }

    pub inline fn asZigString(self: Value) ?[]const u8{
        return switch (self) {
            .OBJECT => |obj| switch (obj.*) {
                .STRING => |str| str.chars,
            },
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
        return self == .STRING;
    }

    pub inline fn isNil(self: Value) bool {
        return self == .NIL;
    }

    pub inline fn isTruthy(self: Value) bool {
        return switch (self) {
            .BOOLEAN => |b| b,
            .NIL => false,
            .NUMBER => |n| n != 0,
            .OBJECT => |obj| switch (obj.*) {
                .STRING => |str| str.chars.len > 0
            },
        };
    }

    pub inline fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| std.debug.print("{}\n", .{b}),
            .NIL => std.debug.print("nil\n", .{}),
            .NUMBER => |n| std.debug.print("{d:6.2}\n", .{n}),
            .OBJECT => |obj| switch (obj.*) {
                .STRING => |str| std.debug.print("{s}\n", .{str.chars})
            },
        };
    }
};

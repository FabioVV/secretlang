const std = @import("std");

pub const ValueType = enum {
    NUMBER,
    BOOLEAN,
    NIL,
    STRING,
};

pub const Value = union(ValueType) {
    NUMBER: f64,
    BOOLEAN: bool,
    NIL: void,
    STRING: []const u8,

    pub inline fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub inline fn createNil() Value {
        return Value{ .NIL = void{} };
    }

    pub inline fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub inline fn createString(str: []const u8) Value {
        return Value{ .STRING = str };
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
            .STRING => true,
        };
    }

    pub inline fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| std.debug.print("{}\n", .{b}),
            .NIL => std.debug.print("nil\n", .{}),
            .NUMBER => |n| std.debug.print("{d:6.2}\n", .{n}),
            .STRING => |s| std.debug.print("{s}\n", .{s}),
        };
    }
};

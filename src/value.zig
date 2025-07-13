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

    pub fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub fn createNil() Value {
        return Value{ .NIL = void{} };
    }

    pub fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub fn createString(str: []const u8) Value {
        return Value{ .STRING = str };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .NUMBER => true,
            else => {
                return false;
            },
        };
    }

    pub fn isBoolean(self: Value) bool {
        return switch (self) {
            .BOOLEAN => true,
            else => {
                return false;
            },
        };
    }

    pub fn isString(self: Value) bool {
        return switch (self) {
            .STRING => true,
            else => {
                return false;
            },
        };
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .BOOLEAN => |b| b,
            .NIL => false,
            .NUMBER => |n| {
                if (n == 0) {
                    return false;
                }
                return true;
            },
            .STRING => true,
        };
    }

    pub fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| std.debug.print("{}\n", .{b}),
            .NIL => std.debug.print("nil\n", .{}),
            .NUMBER => |n| std.debug.print("{d:6.2}\n", .{n}),
            .STRING => |s| std.debug.print("{s}\n", .{s}),
        };
    }
};

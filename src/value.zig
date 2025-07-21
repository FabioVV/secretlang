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
    type:ObjectTypes,

    next: ?*Object,

    data: union(ObjectTypes){
        STRING: *String,
    },

    pub fn init(allocator: std.mem.Allocator, obj_type:ObjectTypes, data: anytype) *Object {
        const obj = allocator.create(Object) catch unreachable;

        obj.* = Object{
            .type = obj_type,
            .data = switch (obj_type) {
                .STRING => .{.STRING = data},
            },
            .next = null,
        };

        return obj;
    }

    pub fn asString(self: *Object) ?*String{
        return switch (self.data) {
            .STRING => |str| str,
        };
    }
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

        const our_string = allocator.dupe(u8, str) catch unreachable;


        const string_obj = String.init(allocator, our_string);
        const obj = Object.init(allocator, .STRING, string_obj);

        return Value{ .OBJECT = obj };
    }

    pub inline fn asZigString(self: Value) ?[]const u8 {
        return switch (self) {
            .OBJECT => |obj| switch (obj.*.data) {
                .STRING => |str| str.chars,
            },
            else => null,
        };
    }

    pub inline fn asObject(self: Value) ?*Object{
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
          .OBJECT => |obj| obj.type == .STRING,
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
            },
        };
    }

    pub inline fn print(self: Value) void {
        return switch (self) {
            .BOOLEAN => |b| std.debug.print("{}\n", .{b}),
            .NIL => std.debug.print("nil\n", .{}),
            .NUMBER => |n| std.debug.print("{d:.2}\n", .{n}),
            .OBJECT => |obj| switch (obj.*.data) {
                .STRING => std.debug.print("{s}\n", .{obj.data.STRING.chars}),
            },
        };
    }
};

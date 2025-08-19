const std = @import("std");
const _value = @import("value.zig");
const Value = _value.Value;
const _vm = @import("vm.zig");

pub const NativeFunctionArityTypes = enum {
    arity0,
    arity1,
};

pub const nativeFunction = struct {
    name: []const u8,
    arity: u8,
    function: union(NativeFunctionArityTypes) {
        arity0: *const fn () Value,
        arity1: *const fn (Value) Value,
    },

    pub fn init(comptime function: anytype, name: []const u8) nativeFunction {
        const ftype = @TypeOf(function);
        const finfo = @typeInfo(ftype).@"fn";
        const arity = finfo.params.len;

        return switch (arity) {
            0 => nativeFunction{
                .function = .{ .arity0 = function },
                .name = name,
                .arity = @intCast(arity),
            },
            1 => nativeFunction{
                .function = .{ .arity1 = function },
                .name = name,
                .arity = @intCast(arity),
            },
            else => unreachable,
        };
    }
};

pub fn _print_(arg: Value) Value {
    arg.print();
    return _vm.NIL;
}

pub const native_functions = [_]nativeFunction{
    nativeFunction.init(_print_, "@print"),
};

pub const NativeFunctionsMap = std.StaticStringMap(void).initComptime(.{
    .{ "@print", void{} },
});

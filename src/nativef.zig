const std = @import("std");
const _value = @import("value.zig");
const NativeFunction = _value.NativeFunction;
const Value = _value.Value;
const _vm = @import("vm.zig");

pub fn _print_(args: []Value) Value {
    for (args) |h| {
        h.print();
    }

    return _vm.NIL;
}

pub const native_functions = [_]NativeFunction{
    .{ .name = "@print", .function = _print_, .arity = 1 },
};

pub const BuiltinMap = std.StaticStringMap(u16).initComptime(.{ // Maps builtin functions to a global index
    .{ "@print", 0 },
});

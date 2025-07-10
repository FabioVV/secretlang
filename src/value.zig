pub const ValueType = enum {
    NUMBER,
    NIL,
    CONSTANT_INDEX,
};

pub const Value = union(ValueType) {
    NUMBER: f64,
    NIL: void,
    CONSTANT_INDEX: u16,

    pub fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub fn createNil() Value {
        return Value{ .NIL = .{} };
    }

    pub fn createConstantIndex(num: u16) Value {
        return Value{ .CONSTANT_INDEX = num };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .NUMBER => true,
            else => {
                return false;
            },
        };
    }

    pub fn asNumber(self: Value) ?f64 {
        return switch (self) {
            .NUMBER => |num| num,
            else => {
                return null;
            },
        };
    }

    pub fn asConstantIndex(self: Value) ?u16 {
        return switch (self) {
            .CONSTANT_INDEX => |num| num,
            else => {
                return null;
            },
        };
    }
};

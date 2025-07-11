pub const ValueType = enum {
    NUMBER,
    BOOLEAN,
    NIL,
};

pub const Value = union(ValueType) {
    NUMBER: f64,
    BOOLEAN: bool,
    NIL: void,

    pub fn createNumber(num: f64) Value {
        return Value{ .NUMBER = num };
    }

    pub fn createNil() Value {
        return Value{ .NIL = .{} };
    }

    pub fn createBoolean(boolean: bool) Value {
        return Value{ .BOOLEAN = boolean };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .NUMBER => true,
            else => {
                return false;
            },
        };
    }

};

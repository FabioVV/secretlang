pub const ValueType = enum {
    NUMBER,
};

pub const Value = union(ValueType) {
    NUMBER: f64,

    pub fn createNumber(num: f64) Value {
        return Value{.NUMBER = num};
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .NUMBER => true,
        };
    }

    pub fn asNumber(self: Value) f64 {
        return switch (self) {
            .NUMBER => |num| num,
        };
    }
};



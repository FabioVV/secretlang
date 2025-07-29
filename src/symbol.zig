const std = @import("std");
const vType = @import("value.zig").ValueType;

pub const Scope = enum {
    GLOBAL,
    LOCAL,
};

pub const Symbol = struct {
    name: []const u8,
    scope: Scope,
    index: u16,
    type: vType,
};

pub const SymbolTable = struct {
    parent_table: ?*SymbolTable,

    table: std.StringHashMap(Symbol),
    total_definitions: u16,

    pub fn init(allocator: std.mem.Allocator) *SymbolTable {
        const st = allocator.create(SymbolTable) catch unreachable;

        st.table = std.StringHashMap(Symbol).init(allocator);
        st.total_definitions = 0;
        st.parent_table = null;

        return st;
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *SymbolTable) *SymbolTable {
        const st = allocator.create(SymbolTable) catch unreachable;

        st.table = std.StringHashMap(Symbol).init(allocator);
        st.total_definitions = 0;
        st.parent_table = outer;

        return st;
    }

    pub fn deinit(self: *SymbolTable) void {
        self.table.deinit();

        if (self.parent_table) |paren_table| {
            paren_table.table.deinit();
            paren_table.deinit();
        }
    }

    pub fn define(self: *SymbolTable, name: []const u8, vtype: vType) Symbol { // Maybe create a defineLocal so it can have register states etc
        var symbol: Symbol = undefined;

        if (self.parent_table != null) {
            symbol = Symbol{ .name = name, .index = self.total_definitions, .scope = .LOCAL, .type = vtype };
        } else {
            symbol = Symbol{ .name = name, .index = self.total_definitions, .scope = .GLOBAL, .type = vtype };
        }

        self.table.put(name, symbol) catch unreachable;
        self.total_definitions += 1;

        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?Symbol {
        if (self.table.get(name)) |s| {
            return s;
        }

        if (self.parent_table) |parent| {
            return parent.resolve(name);
        }

        return null;
    }
};

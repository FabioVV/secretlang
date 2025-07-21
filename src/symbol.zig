pub const std = @import("std");

pub const Scope = enum {
    GLOBAL,
    LOCAL,
};

pub const Symbol = struct {
    name: []const u8,
    scope: Scope,
    index: u16,
};

pub const SymbolTable = struct {
    parent_table: ?*SymbolTable,

    table: std.StringHashMap(Symbol),
    total_definitions: u16,

    pub fn init(allocator: std.mem.Allocator) *SymbolTable {
        const st = allocator.create(SymbolTable) catch unreachable;

        st.table = std.StringHashMap(Symbol).init(allocator);
        st.total_definitions = 0;

        return st;
    }

    pub inline fn define(self: *SymbolTable, name: []const u8) Symbol {
        const symbol = Symbol{.name = name, .index = self.total_definitions, .scope = .LOCAL};

        self.table.put(name, symbol) catch unreachable;
        self.total_definitions += 1;

        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?Symbol {
        if(self.table.get(name)) |s|{
            return s;
        }

        if(self.parent_table) |parent| {
            return parent.resolve(name);
        }

        return null;
    }
};

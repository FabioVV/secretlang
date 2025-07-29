const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

const dbg = @import("debug.zig");
const errh = @import("error.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const _symbol_table = @import("symbol.zig");
const SymbolTable = _symbol_table.SymbolTable;
const Scopes = _symbol_table.Scope;

pub const SemanticAnalyzer = struct {
    ast_program: *AST.Program,
    source: *[]const u8,
    filename: *[]const u8,

    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    symbol_table: *SymbolTable,

    cur_node: AST.CurrentNode,

    had_error: bool,

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, filename: *[]const u8) *SemanticAnalyzer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const sema = arena.allocator().create(SemanticAnalyzer) catch unreachable;

        sema.arena = arena;
        sema.allocator = sema.arena.?.allocator();
        sema.ast_program = ast;
        sema.source = source;
        sema.filename = filename;

        sema.symbol_table = SymbolTable.init(sema.allocator);
        sema.cur_node = undefined;
        sema.had_error = false;

        return sema;
    }

    pub fn repl_init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, filename: *[]const u8, symbol_table: *SymbolTable) *SemanticAnalyzer {
        const sema = allocator.create(SemanticAnalyzer) catch unreachable;

        sema.arena = null;
        sema.allocator = allocator;

        sema.ast_program = ast;
        sema.source = source;
        sema.filename = filename;

        sema.symbol_table = symbol_table;
        sema.cur_node = undefined;
        sema.had_error = false;

        return sema;
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        if (self.arena) |arena| {
            arena.deinit();
        }
    }

    pub fn analyzeVar(self: *SemanticAnalyzer, stmt: AST.VarStatement) void {
        const identifierName = stmt.identifier.literal;

        if (self.symbol_table.resolve(identifierName)) |sb| {
            std.debug.print("error: variable already defined: {s}\n", .{sb.name});
        } else {
            _ = self.symbol_table.define(identifierName, null);
        }
    }

    pub fn analyzeBlock(self: *SemanticAnalyzer, stmt: AST.BlockStatement) void {
        _ = self;
        _ = stmt;
    }

    pub fn analyzeExpressionStmt(self: *SemanticAnalyzer, stmt: AST.ExpressionStatement) void {
        _ = self;
        _ = stmt;
    }

    pub fn analyzeReturn(self: *SemanticAnalyzer, stmt: AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    pub fn analyzeExpression(self: *SemanticAnalyzer, expr: AST.Expression) void {
        _ = self;
        _ = expr;
    }

    pub inline fn analyze_stmt(self: *SemanticAnalyzer, stmt: AST.Statement) void {
        self.cur_node = AST.CurrentNode{ .statement = @constCast(&stmt) };

        switch (stmt) {
            .var_stmt => |var_stmt| {
                self.analyzeVar(var_stmt);
            },
            .return_stmt => |r_stmt| {
                self.analyzeReturn(r_stmt);
            },
            .expression_stmt => |e_stmt| {
                self.analyzeExpressionStmt(e_stmt);
            },
            .block_stmt => |b_stmt| {
                self.analyzeBlock(b_stmt);
            },
        }
    }

    pub fn analyze(self: *SemanticAnalyzer) bool {
        for (self.ast_program.nodes.items) |node| {
            self.analyze_stmt(node);

            if (self.had_error) {
                return false;
            }
        }

        return true;
    }
};

test "semantic analyze test" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();

        if (deinit_status == .leak) {
            @panic("MEMORY LEAK");
        }
    }

    const allocator = gpa.allocator();

    const input: []const u8 =
        \\var a = 5
        \\var b = 10
        \\var c = a + b
        \\c
        \\var c = 1
    ;

    var l: *Lexer = Lexer.init(allocator, input, "<test semantic iterator>");
    defer l.deinit();

    var p: *Parser = Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram(allocator);
    defer program.?.deinit();

    try expect(program != null);
    try expect(!p.had_error);

    var sema: *SemanticAnalyzer = SemanticAnalyzer.init(allocator, program.?, &l.source, &l.filename);
    defer sema.deinit();
    try expect(sema.analyze());
}

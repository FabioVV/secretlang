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
const Symbol = _symbol_table.Symbol;
const Scopes = _symbol_table.Scope;

pub const SemanticAnalyzer = struct {
    ast_program: *AST.Program,
    source: *[]const u8,
    filename: *[]const u8,

    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    symbol_table: *SymbolTable,
    scope_depth: usize,

    cur_node: AST.CurrentNode,

    instruction_count: usize,
    had_error: bool,

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, filename: *[]const u8) *SemanticAnalyzer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const sema = arena.allocator().create(SemanticAnalyzer) catch unreachable;

        sema.arena = arena;
        sema.allocator = sema.arena.?.allocator();
        sema.ast_program = ast;
        sema.source = source;
        sema.filename = filename;

        sema.scope_depth = 0;

        sema.symbol_table = SymbolTable.init(sema.allocator);

        sema.cur_node = undefined;
        sema.instruction_count = 0;
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

        sema.scope_depth = 0;
        sema.symbol_table = symbol_table;

        sema.cur_node = undefined;
        sema.instruction_count = 0;
        sema.had_error = false;

        return sema;
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        if (self.arena) |arena| {
            arena.deinit();
        }
    }

    fn enterScope(self: *SemanticAnalyzer) void {
        self.scope_depth += 1;
        self.symbol_table = SymbolTable.initEnclosed(self.allocator, self.symbol_table);
    }

    fn leaveScope(self: *SemanticAnalyzer) void {
        self.scope_depth -= 1;
        self.symbol_table = self.symbol_table.parent_table.?;
    }

    pub fn analyzeVar(self: *SemanticAnalyzer, stmt: *AST.VarStatement) void {
        if (self.symbol_table.resolveCurrent(stmt.identifier.literal) != null) {
            print("variable already defined: {s}\n", .{stmt.identifier.literal});
            self.had_error = true;
            return;
        }

        const symbol = self.symbol_table.define(stmt.identifier.literal, stmt.token.position.line, null);
        stmt.identifier.resolved_symbol = symbol;

        self.analyzeExpression(stmt.expression);
    }

    pub fn analyzeBlock(self: *SemanticAnalyzer, stmt: *AST.BlockStatement) void {
        self.enterScope();

        _ = self.analyzeStmts(stmt.statements);

        self.leaveScope();
    }

    pub fn analyzeReturn(self: *SemanticAnalyzer, stmt: *AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    pub fn analyzeExpression(self: *SemanticAnalyzer, expr: ?*AST.Expression) void {
        if (expr == null) return;

        switch (expr.?.*) {
            AST.Expression.number_expr => {},
            AST.Expression.string_expr => {},
            AST.Expression.boolean_expr => {},
            AST.Expression.infix_expr => |infixExpr| {
                self.analyzeExpression(infixExpr.left);
                self.analyzeExpression(infixExpr.right);
            },
            AST.Expression.prefix_expr => |infixExpr| {
                self.analyzeExpression(infixExpr.right);
            },
            AST.Expression.if_expr => |ifExpr| {
                self.analyzeExpression(ifExpr.condition);
                self.analyzeBlock(ifExpr.ifBlock.?);

                if (ifExpr.elseBlock) |block| {
                    self.analyzeBlock(block);
                }
            },
            //AST.ArrayExpression => |arr_expr|{

            // },
            AST.Expression.identifier_expr => |*idenExpr| {
                if (self.symbol_table.resolve(idenExpr.literal)) |sb| {
                    idenExpr.resolved_symbol = sb;
                    return;
                }

                print("undefined variable: {s}\n", .{idenExpr.literal});
                self.had_error = true;
            },
            else => unreachable,
        }
    }

    pub fn analyzeExpressionStmt(self: *SemanticAnalyzer, stmt: *AST.ExpressionStatement) void {
        self.analyzeExpression(stmt.expression);
    }

    pub inline fn analyzeStmt(self: *SemanticAnalyzer, stmt: AST.Statement) void {
        self.cur_node = AST.CurrentNode{ .statement = @constCast(&stmt) };
        self.instruction_count += 1;

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

    fn analyzeStmts(self: *SemanticAnalyzer, nodes: std.ArrayList(AST.Statement)) bool {
        for (nodes.items) |node| {
            self.analyzeStmt(node);

            if (self.had_error) {
                return false;
            }
        }

        return true;
    }

    pub fn analyze(self: *SemanticAnalyzer) bool {
        return self.analyzeStmts(self.ast_program.nodes);
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
        \\if(b) {var b = 11}
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

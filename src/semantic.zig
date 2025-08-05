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
const _token = @import("token.zig");
const Token = _token.Token;

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

    /// Emits a compiler error with a custom message
    pub fn sError(self: *SemanticAnalyzer, comptime message: []const u8, varargs: anytype) void {
        self.had_error = true;

        const token: Token = switch (self.cur_node) {
            .statement => |stmt| switch (stmt.*) {
                .var_stmt => |varstmt| varstmt.identifier.token,
                inline else => |v| v.token,
            },
            .expression => |expr| switch (expr.*) {
                inline else => |v| v.token,
            },
        };

        const semanticErrMsg = std.fmt.allocPrint(self.allocator, message, varargs) catch |err| {
            errh.exitWithError("unrecoverable error trying to runtime error message", err);
        };

        const source = dbg.getSourceLine(self.source.*, token.position);
        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, token.position, source);

        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);
        defer self.allocator.free(semanticErrMsg);

        const errMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| semantic error: {s}
            \\
            \\
        , .{ token.position.filename, token.position.line, token.position.column, token.position.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, semanticErrMsg }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write semantic error message", err);
        };

        errh.printError(errMsg);
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
        if (self.symbol_table.resolveCurrent(stmt.identifier.literal)) |s| {
            self.sError("variable '{s}' already defined on line {d}", .{ stmt.identifier.literal, s.token.position.line });
            return;
        }

        self.analyzeExpression(stmt.expression);

        var symbol = self.symbol_table.define(stmt.identifier.token, stmt.identifier.literal, stmt.token.position.line, null);
        stmt.identifier.resolved_symbol = symbol;
    }

    pub fn analyzeFn(self: *SemanticAnalyzer, stmt: *AST.FnStatement) void { // Make this better, store on symbol if it is function or not, so that better error messages may be dispatched here
        if (self.symbol_table.resolveCurrent(stmt.identifier.literal)) |s| {
            self.sError("variable '{s}' already defined on line {d}", .{ stmt.identifier.literal, s.token.position.line });
            return;
        }
        self.enterScope();

        self.analyzeFnBlock(stmt.body.?);

        self.leaveScope();

        var symbol = self.symbol_table.define(stmt.identifier.token, stmt.identifier.literal, stmt.token.position.line, null);
        stmt.identifier.resolved_symbol = symbol;
    }

    pub fn analyzeFnBlock(self: *SemanticAnalyzer, stmt: *AST.BlockStatement) void {
        _ = self.analyzeStmts(stmt.statements);
    }

    pub fn analyzeBlock(self: *SemanticAnalyzer, stmt: *AST.BlockStatement) void {
        self.enterScope();

        _ = self.analyzeStmts(stmt.statements);

        self.leaveScope();
    }

    pub fn analyzeReturn(self: *SemanticAnalyzer, stmt: *AST.ReturnStatement) void {
        self.analyzeExpression(stmt.expression);
    }

    pub fn analyzeExpression(self: *SemanticAnalyzer, expr: ?*AST.Expression) void {
        if (expr == null) return;
        self.cur_node = AST.CurrentNode{ .expression = expr.? };

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
                    self.symbol_table.updateLastUse(idenExpr.literal, self.instruction_count);
                    return;
                }

                self.sError("undefined variable: {s}", .{idenExpr.literal});
            },
            AST.Expression.fn_expr => |fnExpr| {
                self.enterScope();

                self.analyzeFnBlock(fnExpr.body.?);

                self.leaveScope();
            },
            AST.Expression.call_expr => |callExpr| {
                self.analyzeExpression(callExpr.function);
            },
            else => {
                self.sError("unhandled expression during semantic analysis: {any}", .{self.cur_node.expression});
            },
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
            .fn_stmt => |f_stmt| {
                self.analyzeFn(f_stmt);
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
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
        \\var add = fn() { return 1 + 1 }
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

    var a = sema.symbol_table.table.iterator();
    while (a.next()) |e| {
        print("sb: {s} defined in {d}, last use in {any}\n", .{ e.key_ptr.*, e.value_ptr.defined, e.value_ptr.last_use });
    }
}

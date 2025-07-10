const std = @import("std");
const expect = std.testing.expect;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

// todo: Transform values into pointers

pub const Compiler = struct {
    ast_program: *AST.Program,
    instructions: std.ArrayList(Instruction),
    constantsPool: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program) Compiler {
        const compiler = Compiler{ .ast_program = ast, .instructions = std.ArrayList(Instruction).init(allocator), .constantsPool = std.ArrayList(Value).init(allocator)};

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.instructions.deinit();
        self.constantsPool.deinit();
    }

    pub fn addConstant(self: *Compiler, val: Value) u16 {
        self.constantsPool.append(val) catch |err|{
            std.debug.print("{any}\n", .{err});
            std.process.exit(1); // Fix this later
        };

        if(self.constantsPool.items.len > std.math.maxInt(u16)){ // we cant hold more than 65535 constants, because of the bytecode layout
            std.process.exit(1); // Fix this later
        }


        return @intCast(self.constantsPool.items.len);
    }

    pub fn emitConstant(self: *Compiler, val: Value) u16 {
        const contantIndex = self.addConstant(val);

        // allocate register // REMEMBER TO DO LIVENESS ANALYSIS
        self.instructions.append(_instruction.encode_constant(contantIndex, 0)) catch |err|{ // Maybe pass the line so that errors can be nicely reporter later in the vm
            std.debug.print("{any}\n", .{err});
            std.process.exit(1); // Fix this later
        };

        return contantIndex;
    }

    inline fn compileExpression(self: *Compiler, expr: ?*AST.Expression) void {
        switch (expr.?.*) {
            AST.Expression.number_expr => |numExpr| {

                _ = self.emitConstant(Value.createNumber(numExpr.value));

            },
            AST.Expression.infix_expr => |infixExpr| {
                _ = infixExpr;
            },
            else => {},
        }
    }

    inline fn compileVarStatement(self: *Compiler, stmt: AST.VarStatement) void {
        _ = self;
        _ = stmt;
    }

    inline fn compileReturnStatement(self: *Compiler, stmt: AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    inline fn compileExpressionStatement(self: *Compiler, stmt: AST.ExpressionStatement) void {
        self.compileExpression(stmt.expression);
    }

    pub fn compile(self: *Compiler) void {
        for(self.ast_program.nodes.items) |node| {
            switch (node) {
                .var_stmt => |stmt| {
                    self.compileVarStatement(stmt);
                },
                .return_stmt => |stmt| {
                    self.compileReturnStatement(stmt);

                },
                .expression_stmt => |stmt| {
                    self.compileExpressionStatement(stmt);

                },
                else => {
                    // do nothing
                },
            }
        }
    }

};

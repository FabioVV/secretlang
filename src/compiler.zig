const std = @import("std");
const expect = std.testing.expect;

const errorHandling = @import("error.zig");
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

pub const TOTAL_REGISTERS: u8 = 255;

pub const Compiler = struct {
    ast_program: *AST.Program,
    arena: std.heap.ArenaAllocator,
    instructions: std.ArrayList(Instruction),
    constantsPool: std.ArrayList(Value),
    free_registers: std.BoundedArray(u8, TOTAL_REGISTERS),
    used_registers: std.BoundedArray(u8, TOTAL_REGISTERS),

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program) *Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const compiler = arena.allocator().create(Compiler) catch unreachable;

        compiler.*.arena = arena;
        compiler.*.ast_program = ast;
        compiler.*.instructions = std.ArrayList(Instruction).init(compiler.arena.allocator());
        compiler.*.constantsPool = std.ArrayList(Value).init(compiler.arena.allocator());

        compiler.*.free_registers = .{};
        for (0..TOTAL_REGISTERS) |a| {
            compiler.*.free_registers.append(@intCast(a)) catch unreachable;
        }
        compiler.*.used_registers = .{};

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.arena.deinit();
    }

    pub fn addConstant(self: *Compiler, val: Value) u16 {
        self.constantsPool.append(val) catch |err| {
            std.debug.print("{any}\n", .{err});
            std.process.exit(1); // Fix this later
        };

        if (self.constantsPool.items.len > std.math.maxInt(u16)) { // we cant hold more than 65535 constants, because of the bytecode layout
            std.process.exit(1); // Fix this later
        }

        return @intCast(self.constantsPool.items.len - 1);
    }

    pub fn emitInstruction(self: *Compiler, instruction: Instruction) void {
        self.instructions.append(instruction) catch |err| {
            errorHandling.exitWithError("unrecoverable error trying to emit instruction", err);
        };
    }

    pub fn emitConstant(self: *Compiler, val: Value) u16 {
        const contantIndex = self.addConstant(val);
        const result_register = self.free_registers.pop().?;
        self.used_registers.append(result_register) catch unreachable;

        self.instructions.append(_instruction.ENCODE_CONSTANT(contantIndex, result_register)) catch |err| { // Maybe pass the line so that errors can be nicely reporter later in the vm
            errorHandling.exitWithError("unrecoverable error trying to emit constant", err);
        };

        return contantIndex;
    }

    fn compileExpression(self: *Compiler, expr: ?*AST.Expression) void {
        switch (expr.?.*) {
            AST.Expression.number_expr => |numExpr| {
                _ = self.emitConstant(Value.createNumber(numExpr.value));
            },
            AST.Expression.infix_expr => |infixExpr| {
                const operator = infixExpr.token.literal[0];

                self.compileExpression(infixExpr.left);
                self.compileExpression(infixExpr.right);

                const result_register = self.free_registers.pop().?;
                const left_register = self.used_registers.pop().?;
                const right_register = self.used_registers.pop().?;

                self.emitInstruction(_instruction.ENCODE_BINARY(operator, result_register, left_register, right_register));

                self.used_registers.append(result_register) catch unreachable;
                self.free_registers.append(left_register) catch unreachable;
                self.free_registers.append(right_register) catch unreachable;
            },
            else => {},
        }
    }

    fn compileVarStatement(self: *Compiler, stmt: AST.VarStatement) void {
        _ = self;
        _ = stmt;
    }

    fn compileReturnStatement(self: *Compiler, stmt: AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    inline fn compileExpressionStatement(self: *Compiler, stmt: AST.ExpressionStatement) void {
        self.compileExpression(stmt.expression);
    }

    pub fn compile(self: *Compiler) void {
        for (self.ast_program.nodes.items) |node| {
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

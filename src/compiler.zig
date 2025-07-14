const std = @import("std");
const expect = std.testing.expect;

const panic = @import("error.zig");
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

const CompilerError = struct {
    message: []const u8,
};

pub const Compiler = struct {
    ast_program: *AST.Program,
    cur_node: AST.CurrentNode,

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

    /// Emits a compiler error with a custom message
    pub fn cError(self: *Compiler, errorMessage: []const u8) void {
        const token: Token = switch (self.cur_node) {
            .statement => |stmt| switch (stmt.*) {
                inline else => |v| v.token,
            },
            .expression => |expr| switch (expr.*) {
                inline else => |v| v.token,
            },
        };

        const msg = std.fmt.allocPrint(std.heap.page_allocator, "compilation failed: [line {d} column {d}]:\n  {s} \n", .{ token.position.line, token.position.column, errorMessage }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        std.debug.print(" {s}\n", .{msg});
        std.process.exit(1);
    }

    fn registers_status(self: *Compiler) void {
        std.debug.print("FREE REGISTERS: R{d}\n", .{self.free_registers.len});
        std.debug.print("USED REGISTERS: R{d}\n", .{self.used_registers.len});
    }

    fn addConstant(self: *Compiler, val: Value) u16 {
        self.constantsPool.append(val) catch |err| {
            std.debug.print("{any}\n", .{err});
            std.process.exit(1); // Fix this later
        };

        if (self.constantsPool.items.len > std.math.maxInt(u16)) { // we cant hold more than 65535 constants, because of the bytecode layout
            self.cError("exceeded maximum number of constants (65,535)");
        }

        return @intCast(self.constantsPool.items.len - 1);
    }

    fn emitInstruction(self: *Compiler, instruction: Instruction) void {
        self.instructions.append(instruction) catch |err| {
            panic.exitWithError("unrecoverable error trying to emit instruction", err);
        };
    }

    fn emitConstant(self: *Compiler, val: Value) u16 {
        const contantIndex = self.addConstant(val);
        const result_register = self.free_registers.pop().?;
        self.used_registers.append(result_register) catch unreachable;

        self.instructions.append(_instruction.ENCODE_CONSTANT(contantIndex, result_register)) catch |err| { // Maybe pass the line so that errors can be nicely reporter later in the vm
            panic.exitWithError("unrecoverable error trying to emit constant", err);
        };

        return contantIndex;
    }

    fn emitJumpIfFalse(self: *Compiler, ra: u8) usize {
        self.emitInstruction(_instruction.ENCODE_JUMP_IF_FALSE(ra));
        return self.instructions.items.len - 1;
    }

    fn emitJump(self: *Compiler) usize {
        self.emitInstruction(_instruction.ENCODE_JUMP());
        return self.instructions.items.len - 1;
    }

    fn emitNil(self: *Compiler) void {
        const result_register = self.free_registers.pop().?;
        self.used_registers.append(result_register) catch unreachable;
        self.emitInstruction(_instruction.ENCODE_NIL(result_register));
    }

    fn patchJump(self: *Compiler, pos: usize) void {
        const one: usize = 1;
        const jump = self.instructions.items.len - pos - one;

        // ensure the jump doesn't exceed the allowed 18-bit range
        if (jump > 0x3FFFF) { // jump > 18 bits
            self.cError("jump instruction too large");
        }

        self.instructions.items[pos] = self.instructions.items[pos] | (@as(Instruction, @intCast(jump)) & 0x3FFFF);
    }

    fn defineGlobal(self: *Compiler, constantIdx: u16, result_register: u8) void {
        self.emitInstruction(_instruction.ENCODE_DEFINE_GLOBAL(result_register, constantIdx));
    }

    fn compileExpression(self: *Compiler, expr: ?*AST.Expression) void {
        self.cur_node = AST.CurrentNode{ .expression = @constCast(&expr.?.*) };

        switch (expr.?.*) {
            AST.Expression.number_expr => |numExpr| {
                _ = self.emitConstant(Value.createNumber(numExpr.value));
            },
            AST.Expression.string_expr => |strExpr| {
                _ = self.emitConstant(Value.createString(std.heap.page_allocator.dupe(u8, strExpr.value) catch unreachable));
            },
            AST.Expression.boolean_expr => |boolExpr| {
                const result_register = self.free_registers.pop().?;
                self.used_registers.append(result_register) catch unreachable;

                if (boolExpr.value) {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_TRUE(result_register));
                } else {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_FALSE(result_register));
                }
            },
            AST.Expression.infix_expr => |infixExpr| {
                const operator = infixExpr.token.literal;

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
            AST.Expression.prefix_expr => |infixExpr| {
                const operator = infixExpr.token.literal;

                self.compileExpression(infixExpr.right);

                const result_register = self.free_registers.pop().?;
                const right_register = self.used_registers.pop().?;

                self.emitInstruction(_instruction.ENCODE_PREFIX(operator, result_register, right_register));

                self.used_registers.append(result_register) catch unreachable;
                self.free_registers.append(right_register) catch unreachable;
            },
            AST.Expression.if_expr => |ifExpr| {
                self.compileExpression(ifExpr.condition);

                const condtition_register = self.used_registers.pop().?;

                const ifJump = self.emitJumpIfFalse(condtition_register);

                self.free_registers.append(condtition_register) catch unreachable;

                self.compileBlockStatement(ifExpr.ifBlock.?);

                const elseJump = self.emitJump();

                self.patchJump(ifJump);

                if (ifExpr.elseBlock == null) {
                    self.emitNil();
                } else {
                    self.compileBlockStatement(ifExpr.elseBlock.?);
                }

                self.patchJump(elseJump);
            },
            AST.Expression.identifier_expr => |idenExpr| {
                const result_register = self.free_registers.pop().?;
                self.used_registers.append(result_register) catch unreachable;

                const identifierStrIdx = self.addConstant(Value.createString(std.heap.page_allocator.dupe(u8, idenExpr.literal) catch unreachable));

                self.emitInstruction(_instruction.ENCODE_GET_GLOBAL(identifierStrIdx, result_register));
            },
            else => {},
        }
    }

    fn compileVarStatement(self: *Compiler, stmt: AST.VarStatement) void {
        const idenIdx = self.addConstant(Value.createString(std.heap.page_allocator.dupe(u8, stmt.identifier.literal) catch unreachable));

        if (stmt.expression != null) {
            self.compileExpression(stmt.expression);
        } else {
            self.emitNil();
        }

        const result_register = self.used_registers.pop().?;
        self.free_registers.append(result_register) catch unreachable;

        self.defineGlobal(idenIdx, result_register);
    }

    fn compileReturnStatement(self: *Compiler, stmt: AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    fn compileBlockStatement(self: *Compiler, stmt: AST.BlockStatement) void {
        for (stmt.statements.items) |stmt_node| {
            self.compile_stmts(stmt_node);
        }
    }

    inline fn compileExpressionStatement(self: *Compiler, stmt: AST.ExpressionStatement) void {
        self.compileExpression(stmt.expression);
    }

    pub inline fn compile_stmts(self: *Compiler, stmt: AST.Statement) void {
        self.cur_node = AST.CurrentNode{ .statement = @constCast(&stmt) };

        switch (stmt) {
            .var_stmt => |var_stmt| {
                self.compileVarStatement(var_stmt);
            },
            .return_stmt => |r_stmt| {
                self.compileReturnStatement(r_stmt);
            },
            .expression_stmt => |e_stmt| {
                self.compileExpressionStatement(e_stmt);
            },
            .block_stmt => |b_stmt| {
                self.compileBlockStatement(b_stmt);
            },
        }
    }

    pub fn compile(self: *Compiler) void {
        for (self.ast_program.nodes.items) |node| {
            self.compile_stmts(node);

            //self.registers_status();
        }
    }
};

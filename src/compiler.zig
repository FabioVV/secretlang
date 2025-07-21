const std = @import("std");
const print = @import("std").debug.print;
const expect = std.testing.expect;

const dbg = @import("debug.zig");
const panic = @import("error.zig");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const SymbolTable = @import("symbol.zig").SymbolTable;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const String = _value.String;
const Instruction = _instruction.Instruction;
const Opcode = _instruction.Opcode;

// todo: Transform values into pointers

pub const REGISTERS_COUNT: u8 = 255;

const CompilerError = struct {
    message: []const u8,
};

pub const Compiler = struct {
    ast_program: *AST.Program,
    cur_node: AST.CurrentNode,
    source: *[]const u8,

    arena: std.heap.ArenaAllocator,

    instructions: std.ArrayList(Instruction),
    instructions_positions: std.AutoHashMap(u32, Position),

    constantsPool: std.ArrayList(Value),

    strings: std.StringHashMap(Value),

    symbol_table: *SymbolTable,

    free_registers: std.BoundedArray(u8, REGISTERS_COUNT),
    used_registers: std.BoundedArray(u8, REGISTERS_COUNT),


    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8) *Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const compiler = arena.allocator().create(Compiler) catch unreachable;

        compiler.arena = arena;
        compiler.ast_program = ast;
        compiler.source = source;

        compiler.instructions = std.ArrayList(Instruction).init(compiler.arena.allocator());
        compiler.instructions_positions = std.AutoHashMap(u32, Position).init(compiler.arena.allocator());

        compiler.constantsPool = std.ArrayList(Value).init(compiler.arena.allocator());
        compiler.strings = std.StringHashMap(Value).init(compiler.arena.allocator());


        compiler.free_registers = .{};
        for (0..REGISTERS_COUNT) |a| {
            compiler.free_registers.append(@intCast(a)) catch unreachable;
        }
        compiler.used_registers = .{};

        compiler.symbol_table = SymbolTable.init(compiler.arena.allocator());

        return compiler;
    }

    pub fn repl_init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, symbol_table: *SymbolTable) *Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const compiler = arena.allocator().create(Compiler) catch unreachable;

        compiler.arena = arena;
        compiler.ast_program = ast;
        compiler.source = source;

        compiler.instructions = std.ArrayList(Instruction).init(compiler.arena.allocator());
        compiler.instructions_positions = std.AutoHashMap(u32, Position).init(compiler.arena.allocator());

        compiler.constantsPool = std.ArrayList(Value).init(compiler.arena.allocator());
        compiler.strings = std.StringHashMap(Value).init(compiler.arena.allocator());


        compiler.free_registers = .{};
        for (0..REGISTERS_COUNT) |a| {
            compiler.free_registers.append(@intCast(a)) catch unreachable;
        }
        compiler.used_registers = .{};

        compiler.symbol_table = symbol_table;

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.arena.deinit();
    }

    /// Emits a compiler error with a custom message and kills the process
    pub fn cError(self: *Compiler, errorMessage: []const u8) void {
        const token: Token = switch (self.cur_node) {
            .statement => |stmt| switch (stmt.*) {
                inline else => |v| v.token,
            },
            .expression => |expr| switch (expr.*) {
                inline else => |v| v.token,
            },
        };

        const source = dbg.getSourceLine(self.source.*, token);

        const msgLocation = std.fmt.allocPrint(self.arena.allocator(), "{s}In [{s}] {d}:{d}{s}", .{ dbg.ANSI_CYAN, token.position.filename, token.position.line, token.position.column, dbg.ANSI_RESET }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgErrorInfo = std.fmt.allocPrint(self.arena.allocator(), "{s}compilation failed{s}: {s}", .{ dbg.ANSI_RED, dbg.ANSI_RESET, errorMessage }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgSource = std.fmt.allocPrint(self.arena.allocator(), "{s}", .{source}) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        var msgt: []u8 = &[_]u8{};
        for (1..source.len + 1) |idx| {
            //print("{d} : {d}\n", .{ idx, token.position.column });
            if (idx == token.position.column + 1) {
                msgt = std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ msgt, "^" }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            } else {
                msgt = std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ msgt, " " }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            }
        }

        print("\n\n-> {s}\n {d} | {s}\n   | {s}\n   | {s}", .{ msgLocation, token.position.line, msgSource, msgt, msgErrorInfo });
        //std.process.exit(1);
    }

    pub fn canOpError(self: *Compiler, opcode: Opcode) bool {
        _ = self;
        return switch (opcode) {
            .OP_ADD,
            .OP_SUB,
            .OP_MUL,
            .OP_DIV,
            .OP_EQUAL,
            .OP_NOTEQUAL,
            .OP_GREATERTHAN,
            .OP_LESSTHAN,
            .OP_LESSEQUAL,
            .OP_GREATEREQUAL,
            .OP_MINUS,
            .OP_BANG,
            .OP_GET_GLOBAL, // can runtime error
            => true,
            else => false, // won't error (problably)
        };
    }

    fn registers_status(self: *Compiler) void {
        print("FREE REGISTERS: R{d}\n", .{self.free_registers.len});
        print("USED REGISTERS: R{d}\n", .{self.used_registers.len});
    }

    fn addConstant(self: *Compiler, val: Value) u16 {

        const index = self.constantsPool.items.len;

        self.constantsPool.append(val) catch |err| {
            panic.exitWithError("failed to store constant", err);
        };

        if (self.constantsPool.items.len > std.math.maxInt(u16)) { // we cant hold more than 65535 constants, because of the bytecode layout
            self.cError("exceeded maximum number of constants (65,535)");
        }


        return @intCast(index);
    }

    inline fn getCurrentToken(self: *Compiler) Token {
        return switch (self.cur_node) {
            .statement => |stmt| switch (stmt.*) {
                inline else => |v| v.token,
            },
            .expression => |expr| switch (expr.*) {
                inline else => |v| v.token,
            },
        };
    }

    fn emitInstruction(self: *Compiler, instruction: Instruction) void {
        self.instructions.append(instruction) catch |err| {
            panic.exitWithError("unrecoverable error trying to emit instruction", err);
        };

        const opcode = _instruction.GET_OPCODE(instruction);
        if (self.canOpError(opcode)) {
            const token = self.getCurrentToken();
            self.instructions_positions.put(@as(u32, @intCast(self.instructions.items.len - 1)), Position{ .line = token.position.line, .column = token.position.column }) catch |err| {
                panic.exitWithError("failed to store debug for instruction", err);
            };
        }
    }

    fn emitConstant(self: *Compiler, val: Value) u16 {
        const contantIndex = self.addConstant(val);
        const result_register = self.free_registers.pop().?;
        self.used_registers.append(result_register) catch unreachable;

        self.instructions.append(_instruction.ENCODE_LOADK(result_register, contantIndex)) catch |err| {
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

    fn defineGlobal(self: *Compiler, result_register: u8, name: []const u8) void {
        const sb = self.symbol_table.define(name);

        self.emitInstruction(_instruction.ENCODE_DEFINE_GLOBAL(result_register, sb.index));
    }

    fn compileExpression(self: *Compiler, expr: ?*AST.Expression) void {
        self.cur_node = AST.CurrentNode{ .expression = @constCast(&expr.?.*) };

        switch (expr.?.*) {
            AST.Expression.number_expr => |numExpr| {
                _ = self.emitConstant(Value.createNumber(numExpr.value));
            },
            AST.Expression.string_expr => |strExpr| {

                if(self.strings.get(strExpr.value)) |str_ob|{
                    self.arena.allocator().free(strExpr.value);

                    _ = self.emitConstant(str_ob);
                    return;
                }

                _ = self.emitConstant(Value.createString(self.arena.allocator(), strExpr.value));
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
                const operator = infixExpr.token.token_type;

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

//                 const identifierName = self.arena.allocator().dupe(u8, idenExpr.literal) catch unreachable;
//                 _ = self.addConstant(Value.createString(self.arena.allocator(), identifierName)); // Is this necessary?

                if(self.symbol_table.resolve(idenExpr.literal)) |v|{
                    self.emitInstruction(_instruction.ENCODE_GET_GLOBAL(result_register, v.index));
                } else {
                    self.cError("undefined variable");
                }

            },
            else => {},
        }
    }

    fn compileVarStatement(self: *Compiler, stmt: AST.VarStatement) void {
        //const identifierName = self.arena.allocator().dupe(u8, stmt.identifier.literal) catch unreachable;
        //_ = self.addConstant(Value.createString(self.arena.allocator(), identifierName)); // Is this necessary?

        const identifierName = stmt.identifier.literal;

        if (stmt.expression != null) {
            self.compileExpression(stmt.expression);
        } else {
            self.emitNil();
        }

        const result_register = self.used_registers.pop().?;
        self.free_registers.append(result_register) catch unreachable;


        self.defineGlobal(result_register, identifierName);
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

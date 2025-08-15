const std = @import("std");
const print = @import("std").debug.print;
const expect = std.testing.expect;
const assert = std.debug.assert;

const dbg = @import("debug.zig");
const errh = @import("error.zig");
const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;
const Position = _token.Position;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AST = @import("ast.zig");
const _vm = @import("vm.zig");
const _symbol_table = @import("symbol.zig");
const SymbolTable = _symbol_table.SymbolTable;
const Scopes = _symbol_table.Scope;
const Symbol = _symbol_table.Symbol;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const String = _value.String;
const Object = _value.Object;
const ValueTypes = _value.ValueType;
const Instruction = _instruction.Instruction;
const Opcode = _instruction.Opcode;

// todo: Transform values into pointers

const CompilerError = struct {
    message: []const u8,
};

pub const CompilationScope = struct {
    instructions: std.ArrayList(Instruction),
    instructions_positions: std.AutoHashMap(u32, Position),
    registers: std.BoundedArray(bool, _vm.MAX_REGISTERS),
    used_registers: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) CompilationScope {
        var cs = CompilationScope{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .instructions_positions = std.AutoHashMap(u32, Position).init(allocator),
            .registers = std.BoundedArray(bool, _vm.MAX_REGISTERS).init(_vm.MAX_REGISTERS) catch unreachable,
            .used_registers = std.ArrayList(u8).init(allocator),
        };

        @memset(cs.registers.slice(), false);

        return cs;
    }
};

// const ScopeType = enum { MAIN, FUNCTION };

pub const Compiler = struct {
    source: *[]const u8,
    filename: *[]const u8,

    ast_program: *AST.Program,
    cur_node: AST.CurrentNode,

    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    cur_scope: usize,
    scopes: std.ArrayList(CompilationScope),

    constantsPool: std.ArrayList(Value),
    constantsPoolHashes: std.AutoHashMap(u64, u16), // For deduplication of constants

    strings: *std.StringHashMap(Value),
    objects: ?*Object,

    //defined_functions: std.StringHashMap(void),

    had_error: bool,

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, filename: *[]const u8) *Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const compiler = arena.allocator().create(Compiler) catch unreachable;

        compiler.arena = arena;
        compiler.allocator = compiler.arena.?.allocator();
        compiler.ast_program = ast;
        compiler.source = source;
        compiler.filename = filename;

        compiler.cur_scope = 0;
        compiler.scopes = std.ArrayList(CompilationScope).init(compiler.allocator);
        compiler.scopes.append(CompilationScope.init(compiler.allocator)) catch unreachable; // main scope

        compiler.constantsPool = std.ArrayList(Value).init(compiler.allocator);
        compiler.constantsPoolHashes = std.AutoHashMap(u64, u16).init(compiler.allocator);

        compiler.strings = compiler.allocator.create(std.StringHashMap(Value)) catch unreachable;
        compiler.strings.* = std.StringHashMap(Value).init(compiler.allocator);

        compiler.objects = null;
        compiler.cur_node = undefined;
        compiler.had_error = false;

        _ = compiler.addConstant(Value.createNil());
        _ = compiler.addConstant(Value.createBoolean(true));
        _ = compiler.addConstant(Value.createBoolean(false));

        return compiler;
    }

    pub fn repl_init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, filename: *[]const u8, strings_table: *std.StringHashMap(Value)) *Compiler {
        const compiler = allocator.create(Compiler) catch unreachable;

        compiler.arena = null;
        compiler.allocator = allocator;

        compiler.ast_program = ast;
        compiler.source = source;
        compiler.filename = filename;

        compiler.cur_scope = 0;
        compiler.scopes = std.ArrayList(CompilationScope).init(compiler.allocator);
        compiler.scopes.append(CompilationScope.init(compiler.allocator)) catch unreachable; // main scope

        compiler.constantsPool = std.ArrayList(Value).init(compiler.allocator);
        compiler.constantsPoolHashes = std.AutoHashMap(u64, u16).init(compiler.allocator);

        compiler.strings = strings_table;

        compiler.objects = null;
        compiler.cur_node = undefined;
        compiler.had_error = false;

        _ = compiler.addConstant(Value.createNil());
        _ = compiler.addConstant(Value.createBoolean(true));
        _ = compiler.addConstant(Value.createBoolean(false));

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        if (self.arena) |arena| {
            arena.deinit();
        }
    }

    /// Emits a compiler error with a custom message
    pub fn cError(self: *Compiler, errorMessage: []const u8) void {
        self.had_error = true;

        const token: Token = switch (self.cur_node) {
            .statement => |stmt| switch (stmt.*) {
                inline else => |v| v.token,
            },
            .expression => |expr| switch (expr.*) {
                inline else => |v| v.token,
            },
        };

        const source = dbg.getSourceLine(self.source.*, token.position);
        const fmtCaret = dbg.formatSourceLineWithCaret(self.allocator, token.position, source);
        defer self.allocator.free(fmtCaret.caret);
        defer self.allocator.free(fmtCaret.spacing);

        const errMsg = std.fmt.allocPrint(self.allocator,
            \\
            \\-> In [{s}] {d}:{d}
            \\ {d} | {s}
            \\   {s}| {s}
            \\   {s}| compilation failed: {s}
            \\
            \\
        , .{ token.position.filename, token.position.line, token.position.column, token.position.line, source, fmtCaret.spacing, fmtCaret.caret, fmtCaret.spacing, errorMessage }) catch |err| {
            errh.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        errh.printError(errMsg);
    }

    pub fn canOpError(self: *Compiler, opcode: Opcode) bool {
        _ = self;
        return switch (opcode) {
            .ADD,
            .SUB,
            .MUL,
            .DIV,
            .EQUAL,
            .NOTEQUAL,
            .GREATERTHAN,
            .LESSTHAN,
            .LESSEQUAL,
            .GREATEREQUAL,
            .MINUS,
            .BANG,
            .GGLOBAL, // can runtime error
            .CALL,
            => true,
            else => false, // won't error (problably)
        };
    }

    //     fn currentScopeInstructions(self: *Compiler) std.ArrayList(Instruction) {
    //         return self.scopes[self.cur_scope].instructions;
    //     }

    fn registersState(self: *Compiler, op: []const u8) void {
        std.debug.print("\n=== {s} ===\n", .{op});
        std.debug.print("Allocated registers: ", .{});
        for (0.._vm.MAX_REGISTERS) |i| {
            if (self.registers.get(i)) {
                std.debug.print("R{} ", .{i});
            }
        }
        std.debug.print("\n", .{});
    }

    /// allocates a single register for use and returns it, in case of no register available, a OutOfRegisters error is returned
    inline fn allocateRegister(self: *Compiler) !u8 {
        for (1.._vm.MAX_REGISTERS) |i| {
            if (!self.currentScope().registers.get(i)) {
                self.currentScope().registers.set(i, true);
                self.currentScope().used_registers.append(@intCast(i)) catch unreachable;

                if (dbg.DEBUG_REGISTER_ALLOCATION) {
                    print("Allocated R{d}\n", .{i});
                }

                return @intCast(i);
            }
        }

        return error.OutOfRegisters;
    }

    /// free`s the given register
    inline fn freeRegister(self: *Compiler, reg: u8) void {
        if (dbg.DEBUG_REGISTER_ALLOCATION) {
            print("Freed R{d}\n", .{reg});
        }

        if (self.currentScope().registers.get(reg)) {
            self.currentScope().registers.set(reg, false);
        }
    }

    inline fn allocateReturnRegister(self: *Compiler) void {
        self.currentScope().registers.set(0, true);
        self.currentScope().used_registers.append(0) catch unreachable;
    }

    inline fn currentScope(self: *Compiler) *CompilationScope {
        return &self.scopes.items[self.cur_scope];
    }

    inline fn currentScopeInstructions(self: *Compiler) *CompilationScope {
        return &self.scopes.items[self.cur_scope].instructions;
    }

    inline fn currentScopeUsedRegisters(self: *Compiler) *std.ArrayList(u8) {
        return &self.currentScope().used_registers;
    }

    inline fn enterScope(self: *Compiler) void {
        self.scopes.append(CompilationScope.init(self.allocator)) catch unreachable;
        self.cur_scope += 1;
    }

    fn leaveScope(self: *Compiler) ?CompilationScope {
        self.cur_scope -= 1;
        if (self.scopes.pop()) |sc| {
            return sc;
        }
        return null;
    }

    fn addConstant(self: *Compiler, val: Value) u16 {
        const value_hash = val.hash();

        if (self.constantsPoolHashes.get(value_hash)) |v| {
            return v;
        }

        const index = self.constantsPool.items.len;

        self.constantsPool.append(val) catch |err| {
            errh.exitWithError("failed to store constant", err);
        };

        if (self.constantsPool.items.len > std.math.maxInt(u16)) { // we cant hold more than 65535 constants, because of the bytecode layout
            self.cError("exceeded maximum number of constants (65,535)");
        }

        self.constantsPoolHashes.put(value_hash, @intCast(index)) catch unreachable;
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
        self.scopes.items[self.cur_scope].instructions.append(instruction) catch |err| {
            errh.exitWithError("unrecoverable error trying to emit instruction", err);
        };

        const opcode = _instruction.GET_OPCODE(instruction);
        if (self.canOpError(opcode)) {
            const token = self.getCurrentToken();
            self.scopes.items[self.cur_scope].instructions_positions.put(@as(u32, @intCast(self.scopes.items[self.cur_scope].instructions.items.len - 1)), Position{ .line = token.position.line, .column = token.position.column, .filename = self.filename.* }) catch |err| {
                errh.exitWithError("failed to store debug for instruction", err);
            };
        }
    }

    fn emitConstant(self: *Compiler, val: Value) u16 {
        const contantIndex = self.addConstant(val);

        const reg = self.allocateRegister() catch {
            self.cError("out of registers");
            return 0;
        };

        self.scopes.items[self.cur_scope].instructions.append(_instruction.ENCODE_LOADK(reg, contantIndex)) catch |err| {
            errh.exitWithError("unrecoverable error trying to emit constant", err);
        };

        return contantIndex;
    }

    fn emitJumpIfFalse(self: *Compiler, ra: u8) usize {
        self.emitInstruction(_instruction.ENCODE_JUMP_IF_FALSE(ra));
        return self.scopes.items[self.cur_scope].instructions.items.len - 1;
    }

    fn emitJump(self: *Compiler) usize {
        self.emitInstruction(_instruction.ENCODE_JUMP());
        return self.scopes.items[self.cur_scope].instructions.items.len - 1;
    }

    fn emitNil(self: *Compiler) void {
        const reg = self.allocateRegister() catch {
            self.cError("out of registers");
            return;
        };
        self.emitInstruction(_instruction.ENCODE_NIL(reg));
    }

    fn patchJump(self: *Compiler, pos: usize) void {
        const one: usize = 1;
        const jump = self.scopes.items[self.cur_scope].instructions.items.len - pos - one;

        // ensure the jump doesn't exceed the allowed 18-bit range
        if (jump > 0x3FFFF) { // jump > 18 bits
            self.cError("jump instruction too large");
            return;
        }

        self.scopes.items[self.cur_scope].instructions.items[pos] = self.scopes.items[self.cur_scope].instructions.items[pos] | (@as(Instruction, @intCast(jump)) & 0x3FFFF);
    }

    fn compileExpression(self: *Compiler, expr: ?*AST.Expression) ?u16 {
        self.cur_node = AST.CurrentNode{ .expression = @constCast(&expr.?.*) };

        //self.registersState("before expr");
        //defer self.registersState("after expr");

        switch (expr.?.*) {
            AST.Expression.int64_expr => |numExpr| {
                return self.emitConstant(Value.createI64(numExpr.value));
            },
            AST.Expression.float64_expr => |numExpr| {
                return self.emitConstant(Value.createF64(numExpr.value));
            },
            AST.Expression.string_expr => |strExpr| {
                const str = Value.copyString(self.allocator, strExpr.value, self.strings, &self.objects);
                return self.emitConstant(str);
            },
            AST.Expression.boolean_expr => |boolExpr| {
                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return null;
                };

                if (boolExpr.value) {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_TRUE(reg));
                } else {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_FALSE(reg));
                }
            },
            AST.Expression.infix_expr => |infixExpr| {
                const operator = infixExpr.token.token_type;

                _ = self.compileExpression(infixExpr.left);
                _ = self.compileExpression(infixExpr.right);

                const left_register = self.currentScope().used_registers.pop().?;
                const right_register = self.currentScope().used_registers.pop().?;

                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return null;
                };

                self.emitInstruction(_instruction.ENCODE_BINARY(operator, reg, left_register, right_register));

                self.freeRegister(left_register);
                self.freeRegister(right_register);
            },
            AST.Expression.prefix_expr => |infixExpr| {
                const operator = infixExpr.token.literal;

                _ = self.compileExpression(infixExpr.right);

                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return null;
                };
                const right_register = self.currentScope().used_registers.pop().?;

                self.emitInstruction(_instruction.ENCODE_PREFIX(operator, reg, right_register));

                self.freeRegister(right_register);
            },
            AST.Expression.if_expr => |ifExpr| {
                _ = self.compileExpression(ifExpr.condition);

                const cond_register = self.currentScope().used_registers.pop().?;

                const ifJump = self.emitJumpIfFalse(cond_register);

                self.freeRegister(cond_register);

                self.compileBlockStatement(ifExpr.ifBlock.?);

                const elseJump = self.emitJump();

                self.patchJump(ifJump);

                if (ifExpr.elseBlock != null) {
                    self.compileBlockStatement(ifExpr.elseBlock.?);
                } else {
                    self.emitNil();
                }

                self.patchJump(elseJump);
            },
            AST.Expression.fn_expr => |fn_expr| {
                self.enterScope();

                var params_registers = std.BoundedArray(u8, 32).init(0) catch unreachable;
                for (0..fn_expr.parameters.slice().len) |i| {
                    var param = fn_expr.parameters.slice()[i];

                    const local = self.allocateRegister() catch {
                        self.cError("out of registers");
                        return null;
                    };

                    param.resolved_symbol.?.register = local;
                    params_registers.append(local) catch unreachable;

                    //self.emitInstruction(_instruction.ENCODE_NIL(local)); do i need this?
                }

                self.compileBlockStatement(fn_expr.body.?);

                self.emitInstruction(_instruction.ENCODE_RETURN_N());

                const compScope = self.leaveScope().?;

                return self.emitConstant(Value.createFunctionExpr(self.allocator, compScope, params_registers, &self.objects));
            },
            AST.Expression.call_expr => |call_expr| {
                _ = self.compileExpression(call_expr.function); //function here being an identifier that will be resolved

                const fn_register = self.currentScope().used_registers.pop().?;

                const result_reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return null;
                };

                for (call_expr.arguments.slice()) |arg| {
                    _ = self.compileExpression(arg);

                    const arg_reg = self.currentScope().used_registers.pop().?;
                    self.emitInstruction(_instruction.ENCODE_PUSH(arg_reg));
                    self.freeRegister(arg_reg);
                }

                self.freeRegister(fn_register);

                switch (call_expr.function.?.*) {
                    AST.Expression.identifier_expr => |idenExpr| {
                        if (idenExpr.resolved_symbol != null and idenExpr.resolved_symbol.?.type == ValueTypes.NATIVEF) {
                            self.emitInstruction(_instruction.ENCODE_BCALL(result_reg, fn_register, @as(u8, @intCast(call_expr.arguments.slice().len))));

                            return null;
                        }
                    },
                    else => unreachable,
                }

                self.emitInstruction(_instruction.ENCODE_CALL(result_reg, fn_register, @as(u8, @intCast(call_expr.arguments.slice().len))));

                return null;
            },
            AST.Expression.identifier_expr => |idenExpr| {
                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return null;
                };

                if (idenExpr.resolved_symbol) |sym| {
                    if (sym.scope == Scopes.GLOBAL) {
                        self.emitInstruction(_instruction.ENCODE_GET_GLOBAL(reg, sym.index));
                    } else {
                        self.emitInstruction(_instruction.ENCODE_MOVE(reg, sym.register.?));
                    }
                }

                _ = self.addConstant(Value.copyString(self.allocator, idenExpr.literal, self.strings, &self.objects));

                return null;
            },
            else => unreachable,
        }

        return null;
    }

    fn compileVarStatement(self: *Compiler, stmt: *AST.VarStatement) void {
        //         _value.printStdOut("here {any}\n", .{stmt});
        _ = self.compileExpression(stmt.expression);

        //         const maybe_fn = self.constantsPool.items[constantIdx];
        //         if (Value.asFunctionExpr(maybe_fn)) |f| {
        //             _ = f;
        //         }

        const reg = self.currentScope().used_registers.pop().?;
        self.freeRegister(reg);

        //         _ = self.addConstant(Value.copyString(self.allocator, stmt.identifier.literal, self.strings, &self.objects));

        if (stmt.identifier.resolved_symbol) |sym| {
            if (sym.scope == Scopes.GLOBAL) {
                self.emitInstruction(_instruction.ENCODE_DEFINE_GLOBAL(reg, sym.index));
                return;
            } else {
                const local = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return;
                };

                sym.register = local;
                self.emitInstruction(_instruction.ENCODE_MOVE(local, reg));
                return;
            }
        }

        self.cError("PANIC: undefined symbol");
    }

    fn compileReturnStatement(self: *Compiler, stmt: *AST.ReturnStatement) void {
        if (self.cur_scope == 0) {
            self.cError("tried returning from top-level code");
            return;
        }

        _ = self.compileExpression(stmt.expression);

        const reg = self.currentScope().used_registers.pop().?;

        self.emitInstruction(_instruction.ENCODE_RETURN(reg));
        self.freeRegister(reg);
    }

    fn compileBlockStatement(self: *Compiler, stmt: *AST.BlockStatement) void {
        for (stmt.statements.items) |stmt_node| {
            self.compile_stmts(stmt_node);
        }
    }

    inline fn compileExpressionStatement(self: *Compiler, stmt: *AST.ExpressionStatement) void {
        _ = self.compileExpression(stmt.expression);

        if (self.currentScope().used_registers.pop()) |r| {
            self.freeRegister(r);
        }
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
            .fn_stmt => |f_stmt| {
                _ = f_stmt;
            },
        }
    }

    pub fn compile(self: *Compiler) bool {
        for (self.ast_program.nodes.items) |node| {
            self.compile_stmts(node);

            if (self.had_error) {
                return false;
            }
        }

        self.emitInstruction(_instruction.ENCODE_RETURN_N());

        for (0.._vm.MAX_REGISTERS) |i| {
            if (self.currentScope().registers.get(i)) {
                print("R{d} never freed scope: {d}\n", .{ i, self.cur_scope });
                assert(!self.currentScope().registers.get(i)); // All registers should be freed

            }
        }

        return true;
    }
};

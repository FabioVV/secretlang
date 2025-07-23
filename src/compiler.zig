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
const _vm = @import("vm.zig");
const _symbol_table = @import("symbol.zig");
const SymbolTable = _symbol_table.SymbolTable;
const Scopes = _symbol_table.Scope;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const String = _value.String;
const Object = _value.Object;
const Instruction = _instruction.Instruction;
const Opcode = _instruction.Opcode;

// todo: Transform values into pointers


const CompilerError = struct {
    message: []const u8,
};

pub const Compiler = struct {
    ast_program: *AST.Program,
    cur_node: AST.CurrentNode,
    source: *[]const u8,

    allocator: std.mem.Allocator,
    arena: ?std.heap.ArenaAllocator,

    instructions: std.ArrayList(Instruction),
    instructions_positions: std.AutoHashMap(u32, Position),

    constantsPool: std.ArrayList(Value),

    strings: *std.StringHashMap(Value),

    symbol_table: *SymbolTable,
    objects: ?*Object,

    registers: std.BoundedArray(bool, _vm.MAX_REGISTERS),
    current_scope_registers: std.ArrayList(u8),
    //free_registers: std.BoundedArray(u8, _vm.MAX_REGISTERS),
    //used_registers: std.BoundedArray(u8, _vm.MAX_REGISTERS),

    pub fn init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8) *Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const compiler = arena.allocator().create(Compiler) catch unreachable;

        compiler.arena = arena;
        compiler.allocator = compiler.arena.?.allocator();
        compiler.ast_program = ast;
        compiler.source = source;

        compiler.instructions = std.ArrayList(Instruction).init(compiler.allocator);
        compiler.instructions_positions = std.AutoHashMap(u32, Position).init(compiler.allocator);

        compiler.constantsPool = std.ArrayList(Value).init(compiler.allocator);
        compiler.strings = std.StringHashMap(Value).init(compiler.allocator);

        compiler.registers = std.BoundedArray(bool, _vm.MAX_REGISTERS).init(_vm.MAX_REGISTERS) catch unreachable;
        @memset(compiler.registers.slice(), false);
        compiler.current_scope_registers = std.ArrayList(u8).init(compiler.allocator);

        compiler.symbol_table = SymbolTable.init(compiler.allocator);
        compiler.objects = null;
        compiler.cur_node = undefined;

        return compiler;
    }

    pub fn repl_init(allocator: std.mem.Allocator, ast: *AST.Program, source: *[]const u8, symbol_table: *SymbolTable, strings_table: *std.StringHashMap(Value)) *Compiler {
        const compiler = allocator.create(Compiler) catch unreachable;

        compiler.arena = null;
        compiler.allocator = allocator;

        compiler.ast_program = ast;
        compiler.source = source;

        compiler.instructions = std.ArrayList(Instruction).init(compiler.allocator);
        compiler.instructions_positions = std.AutoHashMap(u32, Position).init(compiler.allocator);

        compiler.constantsPool = std.ArrayList(Value).init(compiler.allocator);
        compiler.strings = strings_table;

        compiler.registers = std.BoundedArray(bool, _vm.MAX_REGISTERS).init(_vm.MAX_REGISTERS) catch unreachable;
        @memset(compiler.registers.slice(), false);
        compiler.current_scope_registers = std.ArrayList(u8).init(compiler.allocator);

        compiler.symbol_table = symbol_table;
        compiler.objects = null;
        compiler.cur_node = undefined;

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        if (self.arena) |arena| {
            self.symbol_table.deinit();
            self.allocator.destroy(self.symbol_table);
            arena.deinit();
        }
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

        const msgLocation = std.fmt.allocPrint(self.allocator, "{s}In [{s}] {d}:{d}{s}", .{ dbg.ANSI_CYAN, token.position.filename, token.position.line, token.position.column, dbg.ANSI_RESET }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgErrorInfo = std.fmt.allocPrint(self.allocator, "{s}compilation failed{s}: {s}", .{ dbg.ANSI_RED, dbg.ANSI_RESET, errorMessage }) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        const msgSource = std.fmt.allocPrint(self.allocator, "{s}", .{source}) catch |err| {
            panic.exitWithError("unrecoverable error trying to write parse error message", err);
        };

        var msgt: []u8 = &[_]u8{};
        for (1..source.len + 1) |idx| {
            //print("{d} : {d}\n", .{ idx, token.position.column });
            if (idx == token.position.column + 1) {
                msgt = std.mem.concat(self.allocator, u8, &[_][]const u8{ msgt, "^" }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            } else {
                msgt = std.mem.concat(self.allocator, u8, &[_][]const u8{ msgt, " " }) catch |err| {
                    panic.exitWithError("unrecoverable error", err);
                    return;
                };
            }
        }

        print("\n\n-> {s}\n {d} | {s}\n   | {s}\n   | {s}\n", .{ msgLocation, token.position.line, msgSource, msgt, msgErrorInfo });
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
        for(0.._vm.MAX_REGISTERS) |i|{
            if(!self.registers.get(i)){
                self.registers.set(i, true);
                self.current_scope_registers.append(@intCast(i)) catch unreachable;
                return @intCast(i);
            }
        }

        return error.OutOfRegisters;
    }

    /// free`s the given register
    inline fn freeRegister(self: *Compiler, reg: u8) void {
        self.registers.set(reg, false);
    }

    inline fn freeScopeRegisters(self: *Compiler) void {
        for(self.current_scope_registers.items) |reg| {
            self.freeRegister(reg);
        }

        self.current_scope_registers.clearRetainingCapacity();
    }

    fn enterScope(self: *Compiler) void {
        self.symbol_table = SymbolTable.initEnclosed(self.allocator, self.symbol_table);
    }

    fn leaveScope(self: *Compiler) void {
        self.symbol_table = self.symbol_table.parent_table.?;
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

        const reg = self.allocateRegister() catch {
            self.cError("out of registers");
            return 0;
        };

        self.instructions.append(_instruction.ENCODE_LOADK(reg, contantIndex)) catch |err| {
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
        const reg = self.allocateRegister() catch {
            self.cError("out of registers");
            return;
        };
        self.emitInstruction(_instruction.ENCODE_NIL(reg));
    }

    fn patchJump(self: *Compiler, pos: usize) void {
        const one: usize = 1;
        const jump = self.instructions.items.len - pos - one;

        // ensure the jump doesn't exceed the allowed 18-bit range
        if (jump > 0x3FFFF) { // jump > 18 bits
            self.cError("jump instruction too large");
            return;
        }

        self.instructions.items[pos] = self.instructions.items[pos] | (@as(Instruction, @intCast(jump)) & 0x3FFFF);
    }

    fn defineGlobal(self: *Compiler, result_register: u8, name: []const u8) void {
        const sb = self.symbol_table.define(name);

        if(sb.scope == Scopes.GLOBAL){
            self.emitInstruction(_instruction.ENCODE_DEFINE_GLOBAL(result_register, sb.index));
        } else {
            // define local
        }
    }

    fn compileExpression(self: *Compiler, expr: ?*AST.Expression) void {
        self.cur_node = AST.CurrentNode{ .expression = @constCast(&expr.?.*) };

        self.registersState("before expr");
        defer self.registersState("after expr");

        switch (expr.?.*) {
            AST.Expression.number_expr => |numExpr| {
                _ = self.emitConstant(Value.createNumber(numExpr.value));
            },
            AST.Expression.string_expr => |strExpr| {
                const str = Value.copyString(std.heap.page_allocator, strExpr.value, self.strings, &self.objects);
                _ = self.emitConstant(str);
            },
            AST.Expression.boolean_expr => |boolExpr| {
                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return;
                };

                if (boolExpr.value) {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_TRUE(reg));
                } else {
                    self.emitInstruction(_instruction.ENCODE_BOOLEAN_FALSE(reg));
                }
            },
            AST.Expression.infix_expr => |infixExpr| {
                const operator = infixExpr.token.token_type;

                self.compileExpression(infixExpr.left);
                self.compileExpression(infixExpr.right);

                const left_register = self.current_scope_registers.pop().?;
                const right_register = self.current_scope_registers.pop().?;

                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return;
                };

                self.emitInstruction(_instruction.ENCODE_BINARY(operator, reg, left_register, right_register));

                self.freeRegister(left_register);
                self.freeRegister(right_register);
            },
            AST.Expression.prefix_expr => |infixExpr| {
                const operator = infixExpr.token.literal;

                self.compileExpression(infixExpr.right);

                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return;
                };
                const right_register = self.current_scope_registers.pop().?;

                self.emitInstruction(_instruction.ENCODE_PREFIX(operator, reg, right_register));

                self.freeRegister(right_register);
            },
            AST.Expression.if_expr => |ifExpr| {
                self.compileExpression(ifExpr.condition);

                const cond_register = self.current_scope_registers.pop().?;

                const ifJump = self.emitJumpIfFalse(cond_register);

                self.freeRegister(cond_register);

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
            //AST.ArrayExpression => |arr_expr|{


            // },
            AST.Expression.identifier_expr => |idenExpr| {
                const reg = self.allocateRegister() catch {
                    self.cError("out of registers");
                    return;
                };

                // const identifierName = self.allocator.dupe(u8, idenExpr.literal) catch unreachable;
                // _ = self.addConstant(Value.createString(self.allocator, identifierName)); // Is this necessary?

                if(self.symbol_table.resolve(idenExpr.literal)) |sb| {
                    if(sb.scope == Scopes.GLOBAL){
                        self.emitInstruction(_instruction.ENCODE_GET_GLOBAL(reg, sb.index));

                    } else {
                        // get local
                    }

                    //self.freeRegister(reg);
                    return;
                }

                //self.freeRegister(reg);
                self.cError("undefined variable");
            },
            else => {},
        }
    }

    fn compileVarStatement(self: *Compiler, stmt: AST.VarStatement) void {
        //const identifierName = self.allocator.dupe(u8, stmt.identifier.literal) catch unreachable;
        //_ = self.addConstant(Value.createString(self.allocator, identifierName)); // Is this necessary?

        const identifierName = stmt.identifier.literal;

        if (stmt.expression != null) {
            self.compileExpression(stmt.expression);
        } else {
            self.emitNil();
        }

        const reg = self.current_scope_registers.pop().?;
        self.freeRegister(reg);

        self.defineGlobal(reg, identifierName);
    }

    fn compileReturnStatement(self: *Compiler, stmt: AST.ReturnStatement) void {
        _ = self;
        _ = stmt;
    }

    fn compileBlockStatement(self: *Compiler, stmt: AST.BlockStatement) void {
        self.enterScope();

        for (stmt.statements.items) |stmt_node| {
            self.compile_stmts(stmt_node);
        }

        self.leaveScope();
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
        }
        //          for (0.._vm.MAX_REGISTERS) |i| {
        //              std.debug.assert(!self.registers.get(i)); // All registers should be freed
        //          }

    }
};

const std = @import("std");

const _token = @import("token.zig");
const Position = _token.Position;
const Token = _token.Token;
const Tokens = _token.Tokens;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Compiler = @import("compiler.zig").Compiler;
const AST = @import("ast.zig");
const SymbolTable = @import("symbol.zig").SymbolTable;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const _vm = @import("vm.zig");
const VM = _vm.VM;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

pub fn launch() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const gpa_allocator = gpa.allocator();
    var arena_allocator = std.heap.ArenaAllocator.init(gpa_allocator);
    const allocator = arena_allocator.allocator();

    const globals = std.BoundedArray(Value, _vm.MAX_GLOBALS).init(_vm.MAX_GLOBALS) catch unreachable;
    const symbol_table = SymbolTable.init(allocator);
    var strings = std.StringHashMap(Value).init(allocator);

    defer {
        strings.deinit();
        symbol_table.deinit();

        const deinit_status = gpa.deinit();

        if (deinit_status == .leak) {
            @panic("MEMORY LEAK");
        }

        arena_allocator.deinit();
    }

    var lineCount: u32 = 1;
    while (true) : (lineCount += 1) {
        var buf: [512]u8 = undefined;
        try stdout.print("{d}:>  ", .{lineCount});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: *Lexer = Lexer.init(allocator, input_text, "stdin");

            var p: *Parser = Parser.init(allocator, l);

            const program = try p.parseProgram(allocator);
            defer program.?.deinit();

            if (program == null) {
                try stdout.print("Error parsing program\n", .{});
            }

            if (p.had_error) {
                continue;
            }

            var sema: *SemanticAnalyzer = SemanticAnalyzer.init(allocator, program.?, &l.source, &l.filename);
            defer sema.deinit();

            if (!sema.analyze()) {
                continue;
            }

            var c: *Compiler = Compiler.repl_init(allocator, program.?, &l.source, &l.filename, &strings);

            if (!c.compile()) {
                continue;
            }

            var vm: *VM = VM.repl_init(allocator, &c.constantsPool, &c.scopes.items[0], @constCast(&globals), &l.source, c.strings, c.objects);

            _ = vm.run();
        }
    }
}

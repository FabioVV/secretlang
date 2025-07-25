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
const _vm = @import("vm.zig");
const VM = _vm.VM;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

pub fn launchRepl() !void {
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
        var buf: [2048]u8 = undefined;
        try stdout.print("{d}:>  ", .{lineCount});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: Lexer = try Lexer.init(input_text, "stdin");

            var p: *Parser = Parser.init(&l, allocator);

            const program = try p.parseProgram(allocator);
            defer program.?.deinit();

            if (program == null) {
                try stdout.print("Error parsing program\n", .{});
            }

            if (p.errors.items.len > 0) {
                for (p.errors.items) |err| {
                    try stdout.print("{s}\n", .{err.message});
                }
                continue;
            }

            var c: *Compiler = Compiler.repl_init(allocator, program.?, &l.source, &l.filename, symbol_table, &strings);

            c.compile();

            var vm: *VM = VM.repl_init(allocator, &c.constantsPool, &c.instructions, &c.instructions_positions, @constCast(&globals), &l.source, c.strings, c.objects);

            vm.run();
        }
    }
}

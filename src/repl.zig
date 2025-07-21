const std = @import("std");
const mem = std.mem;
const os = std.os;
const proc = std.process;
const expect = std.testing.expect;

const errorHandling = @import("error.zig");
const debug = @import("debug.zig");
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

    while (true) {
        var buf: [2048]u8 = undefined;
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: Lexer = try Lexer.init(input_text);
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

            var c: *Compiler = Compiler.repl_init(allocator, program.?, &l.source, symbol_table, &strings);

            c.compile();

            //for (program.?.nodes.items) |node| {
            //   debug.printNodes(node);
            //}

            var vm: *VM = VM.repl_init(allocator, &c.constantsPool, &c.instructions, &c.instructions_positions, @constCast(&globals), &l.source, c.strings, c.objects);

            vm.run();
        }
    }
}

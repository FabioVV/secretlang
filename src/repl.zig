const std = @import("std");
const mem = std.mem;
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
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

pub fn launchRepl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var buf: [512]u8 = undefined;
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: Lexer = try Lexer.init(input_text);
            var p: Parser = Parser.init(&l, std.heap.page_allocator);
            defer p.deinit();

            const program = try p.parseProgram(std.heap.page_allocator);
            defer program.?.deinit();

            if (program == null) {
                try stdout.print("Error parsing program\n", .{});
            }

            if (p.errors.items.len > 0) {
                for (p.errors.items) |err| {
                    try stdout.print("\n{s}\n", .{err.message});
                }
                continue;
            }

            var c: Compiler = Compiler.init(std.heap.page_allocator, program.?);
            defer c.deinit();

            //const bytecode = c.compile();
            c.compile();

            const op = _instruction.getOpcode(c.instructions.items[0]);
            std.debug.print("Opcode: {any}\n", .{op});
            std.debug.print("Number val: {d:6.2}\n", .{c.constantsPool.items[0].asNumber()});

            // for (program.?.nodes.items) |node| {
            //    debug.printNodes(node);
            // }
        }
    }
}

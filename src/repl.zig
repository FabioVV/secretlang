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
const AST = @import("ast.zig");

pub fn launchRepl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var buf: [512]u8 = undefined;
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: Lexer = try Lexer.init(input_text);
            var p: Parser = try Parser.init(&l, std.heap.page_allocator);
            defer p.deinit();

            const program = try p.parseProgram(std.heap.page_allocator);
            defer program.?.deinit();

            if (program == null) {
                try stdout.print("Error parsing program: program is null\n", .{});
            }

            if (p.errors.items.len > 0) {
                for (p.errors.items) |err| {
                    try stdout.print("\n{s}\n", .{err.message});
                }
                continue;
            }

            for (program.?.nodes.items) |node| {
                debug.printNodes(node);
            }
        }
    }
}

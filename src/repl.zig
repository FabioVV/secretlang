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
const _vm = @import("vm.zig");
const VM = _vm.VM;
const _instruction = @import("instruction.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Instruction = _instruction.Instruction;

extern "kernel32" fn ReadConsoleW(handle: os.fd_t, buffer: [*]u16, len: os.windows.DWORD, read: *os.windows.DWORD, input_ctrl: ?*void) i32;
extern "kernel32" fn SetConsoleOutputCP(cp: os.windows.UINT) i32;

pub fn launchRepl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var globalStore = std.StringHashMap(Value).init(std.heap.page_allocator);

    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            @panic("MEMORY LEAK");
        }
    }

    const allocator = gpa.allocator();

    while (true) {
        var buf: [2048]u8 = undefined;
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |input_text| {
            var l: Lexer = try Lexer.init(input_text);
            var p: *Parser = Parser.init(&l, allocator);
            defer p.deinit();

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

            var c: *Compiler = Compiler.init(allocator, program.?, &l.source);
            defer c.deinit();

            c.compile();

            //for (program.?.nodes.items) |node| {
            //   debug.printNodes(node);
            //}

            var vm: *VM = VM.repl_init(allocator, &c.*.constantsPool, &c.*.instructions, &c.instructions_positions, globalStore);
            defer vm.deinit();

            vm.run();

            // Workaround for globals in the VM, i copy them back and forth here and in the VM so that the REPL session can keep track of variables, functions etc.
            // I choose this over allocating memory for the globals in the heap for performance reasons, which while negligible in the REPL, it does matters in normal programs
            globalStore.deinit();
            globalStore = vm.*.globals.clone() catch unreachable;
        }
    }
}

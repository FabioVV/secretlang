const std = @import("std");
const io = std.io;
const builtin = @import("builtin");
const print = std.debug.print;

const REPL = @import("repl.zig");
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

fn execute(allocator: std.mem.Allocator, file: []const u8, filename: []const u8) !void {
    const stdout = io.getStdOut().writer();

    var l: Lexer = try Lexer.init(file, filename);

    var p: *Parser = Parser.init(&l, allocator);
    defer p.deinit();

    const program = try p.parseProgram(allocator);
    if (program == null) {
        try stdout.print("Error parsing program\n", .{});
        return;
    }

    defer program.?.deinit();

    if (p.had_error) {
        return;
    }

    var c: *Compiler = Compiler.init(allocator, program.?, &l.source, &l.filename);
    c.compile();
    defer c.deinit();

    var vm: *VM = VM.init(allocator, &c.constantsPool, &c.instructions, &c.instructions_positions, &l.source, c.strings, c.objects);
    vm.run();
    defer vm.deinit();
}

fn readFileContents(allocator: std.mem.Allocator, filepath: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const fileSize = try file.getEndPos();
    const contents = try allocator.alloc(u8, fileSize);
    _ = try file.readAll(contents);

    return contents;
}

pub fn runFromFile(allocator: std.mem.Allocator, filepath: []const u8, filename: []const u8) !void {
    const fileContent = readFileContents(allocator, filepath) catch |err| switch (err) {
        error.FileNotFound => {
            print("Error: File '{s}' not found\n", .{filepath});
            return;
        },
        error.AccessDenied => {
            print("Error: Permission denied reading '{s}'\n", .{filepath});
            return;
        },
        else => {
            print("Error reading file '{s}': {}\n", .{ filepath, err });
            return;
        },
    };
    const trimmedFileContent = std.mem.trim(u8, fileContent, &std.ascii.whitespace);

    defer allocator.free(fileContent);

    execute(allocator, trimmedFileContent, filename) catch |err| {
        print("Execution error: {}\n", .{err});
        return;
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();

        if (deinit_status == .leak) {
            @panic("MEMORY LEAK");
        }
    }

    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        const binary_name = std.fs.path.basename(args[0]);
        print("usage: {s} <filepath> [options] \n", .{binary_name});
        print("     or: {s} --repl\n", .{binary_name});
        return;
    }

    if (std.mem.eql(u8, args[1], "--repl") or std.mem.eql(u8, args[1], "-r")) {
        try REPL.launchRepl();
        return;
    }

    const filepath = args[1];
    const filename = std.fs.path.basename(args[1]);
    try runFromFile(allocator, filepath, filename);
}

//
// fn parseArgs(allocator: Allocator, args: [][]const u8) !struct {
//     filename: ?[]const u8 = null,
//     repl_mode: bool = false,
//     debug_mode: bool = false,
//     show_tokens: bool = false,
//     show_ast: bool = false,
//     show_bytecode: bool = false,
// } {
//     var result = .{};
//
//     var i: usize = 1;
//     while (i < args.len) : (i += 1) {
//         const arg = args[i];
//
//         if (std.mem.eql(u8, arg, "--repl") or std.mem.eql(u8, arg, "-r")) {
//             result.repl_mode = true;
//         } else if (std.mem.eql(u8, arg, "--debug") or std.mem.eql(u8, arg, "-d")) {
//             result.debug_mode = true;
//         } else if (std.mem.eql(u8, arg, "--tokens")) {
//             result.show_tokens = true;
//         } else if (std.mem.eql(u8, arg, "--ast")) {
//             result.show_ast = true;
//         } else if (std.mem.eql(u8, arg, "--bytecode")) {
//             result.show_bytecode = true;
//         } else if (!std.mem.startsWith(u8, arg, "-")) {
//             // This is the filename
//             result.filename = arg;
//         } else {
//             print("Unknown option: {s}\n", .{arg});
//             return error.InvalidArgument;
//         }
//     }
//
//     return result;
// }

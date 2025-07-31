const std = @import("std");
const builtin = @import("builtin");

const REPL = @import("repl.zig");
const errh = @import("error.zig");
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

const ArgsConfig = struct {
    repl_mode: bool = false,
};

fn execute(allocator: std.mem.Allocator, file: []const u8, filename: []const u8) !void {
    var l: *Lexer = Lexer.init(allocator, file, filename);
    defer l.deinit();

    var p: *Parser = Parser.init(allocator, l);
    defer p.deinit();

    var program = try p.parseProgram(allocator);
    if (program == null) {
        errh.printError("error parsing program\n");
        return;
    }

    defer program.?.deinit();

    if (p.had_error) {
        return;
    }

    var sema: *SemanticAnalyzer = SemanticAnalyzer.init(allocator, program.?, &l.source, &l.filename);
    defer sema.deinit();

    if (!sema.analyze()) {
        return;
    }

    var c: *Compiler = Compiler.init(allocator, program.?, &l.source, &l.filename);
    defer c.deinit();

    if (!c.compile()) {
        return;
    }

    var vm: *VM = VM.init(allocator, &c.constantsPool, &c.scopes.items[0], &l.source, c.strings, c.objects);
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
            errh.printErrorComptime(allocator, "Error: Could not open file. File '{s}' not found\n", .{filepath});
            return;
        },
        error.AccessDenied => {
            errh.printErrorComptime(allocator, "Error: Permission denied reading '{s}'\n", .{filepath});
            return;
        },
        else => {
            errh.printErrorComptime(allocator, "Error reading file '{s}': {}\n", .{ filepath, err });
            return;
        },
    };
    //const trimmedFileContent = std.mem.trim(u8, fileContent, &std.ascii.whitespace);

    defer allocator.free(fileContent);

    execute(allocator, fileContent, filename) catch |err| {
        errh.printErrorComptime(allocator, "Execution error: {}\n", .{err});
        return;
    };
}

fn parseArgsConfig(allocator: std.mem.Allocator, args: [][:0]u8) !ArgsConfig {
    var result = ArgsConfig{};

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "--repl") or std.mem.eql(u8, arg, "-r")) {
            result.repl_mode = true;
        } else {
            errh.printErrorComptime(allocator, "Unknown option: {s}\n", .{arg});
            return error.InvalidArgument;
        }
    }

    return result;
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
        errh.printErrorComptime(allocator, "usage: {s} <filepath> [options] \n", .{binary_name});
        errh.printErrorComptime(allocator, "     or: {s} --repl\n", .{binary_name});
        return;
    }

    if (std.mem.eql(u8, args[1], "--repl") or std.mem.eql(u8, args[1], "-r")) {
        try REPL.launch();
        return;
    }

    //     _ = parseArgsConfig(args) catch |err| switch (err) {
    //         error.InvalidArgument => {
    //             return;
    //         }
    //     };

    const filepath = args[1];
    const filename = std.fs.path.basename(args[1]);
    try runFromFile(allocator, filepath, filename);
}

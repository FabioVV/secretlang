const std = @import("std");
const builtin = @import("builtin");
const print = std.debug.print;

const REPL = @import("repl.zig");

fn executeFromFile(allocator: std.mem.Allocator, file: []const u8) !void {
    //  execute
}

fn readFileContents(allocator: std.mem.Allocator, filepath: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const fileSize = try file.getEndPos();
    const contents = try allocator.alloc(u8, fileSize);
    _ = try file.readAll(contents);

    return contents;
}

pub fn runFromFile(allocator: std.mem.Allocator, filepath: []const u8) !void {
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

    defer allocator.free(fileContent);

    executeFromFile(allocator, fileContent) catch |err| {
        print("Execution error: {}\n", .{err});
        return;
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        print("usage: {s} <filepath> [options] \n", .{args[0]});
        print("     or: {s} --repl\n", .{args[0]});
        return;
    }

    if (std.mem.eql(u8, args[1], "--repl") or std.mem.eql(u8, args[1], "--r")) {
        try REPL.launchRepl();
        return;
    }

    const filepath = args[1];
    try runFromFile(filepath);
}

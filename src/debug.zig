const std = @import("std");
const io = std.io;

const _token = @import("token.zig");
const Token = _token.Token;
const Tokens = _token.Tokens;

const DEBUG: bool = false; // If enabled, debug fns are going to run wherever they are called

pub fn printToken(token: Token) void {
    const stdoutwriter = io.getStdOut().writer();

    if (!DEBUG) return;

    stdoutwriter.print("Token: {s} - Literal: {s}\n", .{ @tagName(token.token_type), token.literal }) catch |err| {
        std.debug.print("Error debug printing token: {any}", .{err});
    };
}

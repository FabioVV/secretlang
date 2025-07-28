const std = @import("std");

pub fn exitWithError(message: []const u8, err: anyerror) noreturn {
    std.log.err("\n{s}: {s} ->{any}\n\n", .{ message, @errorName(err), err });
    std.process.exit(1);
}

pub fn printError(message: []const u8) void {
    _ = std.io.getStdErr().write(message) catch unreachable;
}

pub fn printErrorComptime(allocator: std.mem.Allocator, comptime message: []const u8, varargs: anytype) void {
    const errMsg = std.fmt.allocPrint(allocator, message, varargs) catch unreachable;

    _ = std.io.getStdErr().write(errMsg) catch unreachable;
    allocator.free(errMsg);
}

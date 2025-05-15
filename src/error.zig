const std = @import("std");

pub fn exitWithError(message: []const u8, err: anyerror) noreturn {
    std.log.err("\n{s}: {s} ->{any}\n\n", .{ message, @errorName(err), err });
    std.process.exit(1);
}

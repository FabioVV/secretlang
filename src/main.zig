const std = @import("std");
const builtin = @import("builtin");
const REPL = @import("repl.zig");

pub fn main() !void {
    try REPL.launchRepl();
}

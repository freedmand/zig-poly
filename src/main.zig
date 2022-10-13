const std = @import("std");
const reader = @import("reader.zig");

pub fn main() anyerror!void {
    var stringReader = parser.StringReader{ .context = parser.StringReaderContext{ .buffer = "hello" } };

    std.log.err("String read: {s}", .{try stringReader.read(5)});
}

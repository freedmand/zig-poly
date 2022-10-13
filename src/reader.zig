const std = @import("std");
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const testing = std.testing;
const eql = std.mem.eql;

pub const ReadError = error{
    /// The offset results in a negative position
    NegativePosition,
    /// The offset results in overflow
    OverflowPosition,
    /// Out of memory error
    OutOfMemory
};

pub fn ReadResult(comptime Context: type) type {
    return struct {
        result: []const u8,
        context: Context,
    };
}

pub fn AdvanceResult(comptime Context: type) type {
    return struct {
        bytesRead: usize,
        context: Context,
    };
}

/// A reader "reads" through a string buffer, keeping track of position
/// and offering methods to peek/advance.
pub fn Reader(
    comptime Context: type,
    comptime readFn: fn (context: Context, size: usize, offset: isize, updatePosition: bool) ReadError!ReadResult(Context),
    comptime advanceFn: fn (context: Context, size: usize) AdvanceResult(Context),
) type {
    return struct {
        /// The internal context for the reader
        context: Context,
        /// The allocator to use to allocate memory
        allocator: std.mem.Allocator,

        const Self = @This();
        const ReaderReadResult = struct {
            result: []const u8,
            reader: Self,
        };
        const ReaderAdvanceResult = struct {
            bytesRead: usize,
            reader: Self,
        };

        fn readOrPeekWithOffset(self: Self, size: usize, offset: isize, isRead: bool) ReadError!ReaderReadResult {
            const result = try readFn(self.context, size, offset, isRead);
            return ReaderReadResult{
                .result = result.result,
                .reader = Self{
                    .context = result.context,
                    .allocator = self.allocator,
                },
            };
        }

        /// Read from the internal buffer {size} bytes {offset}
        pub fn readWithOffset(self: Self, size: usize, offset: isize) ReadError!ReaderReadResult {
            return self.readOrPeekWithOffset(size, offset, true);
        }

        /// Read from the internal buffer {size} bytes (with no offset)
        pub fn read(self: Self, size: usize) ReadError!ReaderReadResult {
            return self.readOrPeekWithOffset(size, 0, true);
        }

        /// Peek from the internal buffer {size} bytes {offset}
        pub fn peekWithOffset(self: Self, size: usize, offset: isize) ReadError!ReaderReadResult {
            return self.readOrPeekWithOffset(size, offset, false);
        }

        /// Peek from the internal buffer {size} bytes (with no offset)
        pub fn peek(self: Self, size: usize) ReadError!ReaderReadResult {
            return self.readOrPeekWithOffset(size, 0, false);
        }

        /// Advance {size} bytes and return the number of bytes advanced
        pub fn advance(self: Self, size: usize) ReaderAdvanceResult {
            const result = advanceFn(self.context, size);
            return ReaderAdvanceResult{
                .bytesRead = result.bytesRead,
                .reader = Self{
                    .context = result.context,
                    .allocator = self.allocator,
                },
            };
        }
    };
}

/// The internal context for string reader
pub const StringReaderContext = struct {
    /// The current position in bytes
    position: usize = 0,
    /// The internal buffer
    buffer: []const u8,

    pub fn withPosition(self: StringReaderContext, position: usize) StringReaderContext {
        return StringReaderContext{
            .position = position,
            .buffer = self.buffer,
        };
    }
};

/// The string reader function reads a slice from the internal buffer
fn stringReaderRead(context: StringReaderContext, size: usize, offset: isize, updatePosition: bool) ReadError!ReadResult(StringReaderContext) {
    var position: isize = undefined;
    if (@addWithOverflow(isize, @intCast(isize, context.position), offset, &position)) {
        return error.OverflowPosition;
    }
    if (position < 0) {
        return error.NegativePosition;
    }
    const endPosition = std.math.min(@intCast(usize, position) + size, context.buffer.len);

    // Create new context object
    const newContext = if (updatePosition) context.withPosition(endPosition) else context;

    return ReadResult(StringReaderContext){
        .result = context.buffer[@intCast(usize, position)..@intCast(usize, endPosition)],
        .context = newContext,
    };
}

/// The string advance function advances the position a number of bytes, returning how many bytes it actually advanced
fn stringReaderAdvance(context: StringReaderContext, size: usize) AdvanceResult(StringReaderContext) {
    const endPosition = std.math.min(context.position + size, context.buffer.len);
    // Number of bytes read
    const bytesRead = endPosition - context.position;

    // Create new context object
    const newContext = context.withPosition(endPosition);

    return AdvanceResult(StringReaderContext){
        .bytesRead = bytesRead,
        .context = newContext,
    };
}

pub const StringReader = Reader(StringReaderContext, stringReaderRead, stringReaderAdvance);

test "string reader peek" {
    var stringReader = StringReader{ .context = StringReaderContext{ .buffer = "hello" }, .allocator = testing.allocator };
    try expect(eql(u8, (try stringReader.peek(0)).result, ""));
    try expect(eql(u8, (try stringReader.peek(3)).result, "hel"));
    try expect(eql(u8, (try stringReader.peek(5)).result, "hello"));
    try expect(eql(u8, (try stringReader.peek(100)).result, "hello"));
    try expect(eql(u8, (try stringReader.peekWithOffset(100, 2)).result, "llo"));
    try expectError(error.NegativePosition, stringReader.peekWithOffset(0, -10));
    try expectError(error.NegativePosition, stringReader.peekWithOffset(3, -1));
}

test "string reader read" {
    var stringReader = StringReader{ .context = StringReaderContext{ .buffer = "hello" }, .allocator = testing.allocator };

    var result = try stringReader.read(0);
    try expect(eql(u8, result.result, ""));
    // Multiple calls don't advance if you don't save the new reader
    try expect(eql(u8, (try stringReader.read(5)).result, "hello"));
    try expect(eql(u8, (try stringReader.read(5)).result, "hello"));
    stringReader = result.reader;

    result = try stringReader.read(3);
    try expect(eql(u8, result.result, "hel"));
    stringReader = result.reader;

    result = try stringReader.read(5);
    try expect(eql(u8, result.result, "lo"));
    stringReader = result.reader;

    result = try stringReader.read(100);
    try expect(eql(u8, result.result, ""));
    stringReader = result.reader;

    result = try stringReader.readWithOffset(100, -4);
    try expect(eql(u8, result.result, "ello"));
    stringReader = result.reader;

    result = try stringReader.readWithOffset(100, -5);
    try expect(eql(u8, result.result, "hello"));
    stringReader = result.reader;

    result = try stringReader.readWithOffset(100, 0);
    try expect(eql(u8, result.result, ""));
    stringReader = result.reader;

    try expectError(error.NegativePosition, stringReader.readWithOffset(0, -10));
    try expectError(error.NegativePosition, stringReader.readWithOffset(3, -6));
}

test "string reader advance" {
    var stringReader = StringReader{ .context = StringReaderContext{ .buffer = "hello" }, .allocator = testing.allocator };

    var result = stringReader.advance(2);
    try expect(result.bytesRead == 2);

    try expect(eql(u8, (try stringReader.peek(2)).result, "he"));
    stringReader = result.reader;
    try expect(eql(u8, (try stringReader.peek(2)).result, "ll"));

    result = stringReader.advance(2);
    stringReader = result.reader;
    try expect(result.bytesRead == 2);

    try expect(eql(u8, (try stringReader.peek(2)).result, "o"));

    result = stringReader.advance(2);
    stringReader = result.reader;
    try expect(result.bytesRead == 1);

    try expect(eql(u8, (try stringReader.peek(2)).result, ""));

    try expect(stringReader.advance(2).bytesRead == 0);
}

const std = @import("std");
const ReaderLib = @import("reader.zig");
const eql = std.mem.eql;
const expect = std.testing.expect;
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

/// A successful parse result
pub fn ParseResultSuccess(comptime Reader: type, comptime Result: type) type {
    return struct {
        /// The reader (with position advanced to represent the parse result)
        reader: Reader,
        /// The parse result
        result: Result,
    };
}

/// A parse result, which can be successful or failing
pub fn ParseResult(comptime Reader: type, comptime Result: type) type {
    return union(enum) {
        /// A successful parse
        success: ParseResultSuccess(Reader, Result),
        /// A failing parse
        fail: void,
    };
}

/// A utility function to grab the type of a parse function
fn parseFnType(comptime Result: type, comptime Context: type) type {
    return @TypeOf(struct {
        fn func(_: Context, reader: anytype, _: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), Result) {
            unreachable;
        }
    }.func);
}

/// A parser which can consume some content
pub fn Parser(
    /// Context which is passed to the parse function
    comptime context: anytype,
    /// The result type for the parse
    comptime Result: type,
    /// The function that performs the parse
    comptime parseFn: parseFnType(Result, @TypeOf(context)),
) type {
    return struct {
        /// The type of the result
        pub const ResultType = Result;

        /// Parse from a given reader
        pub fn parse(_: @This(), reader: anytype) ReaderLib.ReadError!ParseResult(@TypeOf(reader), Result) {
            return parseFn(context, reader, reader.allocator);
        }

        /// Parse from a given reader with a custom allocator
        pub fn parseWithAllocator(_: @This(), reader: anytype, allocator: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), Result) {
            return parseFn(context, reader, allocator);
        }
    };
}

/// A utility function to extract the result type from a parser
fn ParserType(parser: anytype) type {
    return @field(@TypeOf(parser), "ResultType");
}

fn OneOfParserType(parsers: anytype) type {
    assert(parsers.len > 0); // need to have at least one parser
    // Extract the first parser
    var T = ParserType(parsers[0]);
    var first = true;

    // Loop through the rest of the parsers and assert they're equal
    inline for (parsers) |parser| {
        if (first) {
            // Skip first, since we already extracted that type
            first = false;
            continue;
        } else {
            assert(ParserType(parser) == T); // all the parsers must have the same type
        }
    }

    return T;
}

/// A utility function to extract the result type from an anonymous tuple of parsers
fn ParsersType(parsers: anytype) type {
    var tuple_fields: [parsers.len]std.builtin.Type.StructField = undefined;
    inline for (parsers) |parser, i| {
        const T = ParserType(parser);
        var num_buf: [128]u8 = undefined;
        tuple_fields[i] = std.builtin.Type.StructField{
            .name = std.fmt.bufPrint(&num_buf, "{d}", .{i}) catch unreachable,
            .field_type = T,
            .default_value = @as(?*const anyopaque, null),
            .is_comptime = false,
            .alignment = if (@sizeOf(T) > 0) @alignOf(T) else 0,
        };
    }

    return @Type(std.builtin.Type{
        .Struct = std.builtin.Type.Struct{
            .is_tuple = true,
            .layout = .Auto,
            .decls = &[_]std.builtin.Type.Declaration{},
            .fields = &tuple_fields,
        },
    });
}

/// A function to parse a specific literal
pub fn literalParseFn(literal: []const u8, reader: anytype, _: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), []const u8) {
    const Result = ParseResult(@TypeOf(reader), []const u8);
    const SuccessResult = ParseResultSuccess(@TypeOf(reader), []const u8);

    const literalLen = literal.len;

    const readResult = try reader.read(literalLen);
    if (readResult.result.len == literalLen and eql(u8, readResult.result, literal)) {
        // Parse succeeds if the exact literal can be read
        return Result{
            .success = SuccessResult{ .reader = readResult.reader, .result = readResult.result },
        };
    }
    // Parse fails otherwise
    return Result{ .fail = {} };
}

/// A literal parser can read a specific string of text
pub fn LiteralParser(comptime literal: []const u8) Parser(literal, []const u8, literalParseFn) {
    return Parser(literal, []const u8, literalParseFn){};
}

/// A function to parse one of multiple different options
pub fn oneOfParseFn(parsers: anytype, reader: anytype, allocator: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), OneOfParserType(parsers)) {
    inline for (parsers) |parser| {
        // Try to parse with each parser
        var arena = ArenaAllocator.init(allocator);
        const parseResult = try parser.parseWithAllocator(reader, arena.allocator());
        if (parseResult == .success) {
            // Return at the first success
            return parseResult;
        }
        arena.deinit();
    }
    // If no parsers succeeded, it's a fail
    return .fail;
}

/// A one-of parser tries multiple parsers, taking the first that succeeds
pub fn OneOfParser(comptime parsers: anytype) Parser(parsers, OneOfParserType(parsers), oneOfParseFn) {
    return Parser(parsers, OneOfParserType(parsers), oneOfParseFn){};
}

/// A function to optionally parse a value; succeeds either way
pub fn optionalParseFn(parser: anytype, reader: anytype, allocator: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), ?(ParserType(parser))) {
    const ResultType = ?(ParserType(parser));
    const Result = ParseResult(@TypeOf(reader), ResultType);
    const SuccessResult = ParseResultSuccess(@TypeOf(reader), ResultType);

    var arena = ArenaAllocator.init(allocator);
    const parseResult = try parser.parseWithAllocator(reader, arena.allocator());
    if (parseResult == .success) {
        // If the parse succeeds, great
        return Result{
            .success = SuccessResult{
                .reader = parseResult.success.reader,
                .result = parseResult.success.result,
            },
        };
    }
    arena.deinit();
    // If the parse fails, that's ok too
    return Result{ .success = SuccessResult{ .reader = reader, .result = null } };
}

/// An optional parser tries to parse a value, succeeding either way
pub fn OptionalParser(comptime parser: anytype) Parser(parser, ?(ParserType(parser)), optionalParseFn) {
    return Parser(parser, ?(ParserType(parser)), optionalParseFn){};
}

/// A function to parse zero or more instances of a value
pub fn zeroOrMoreParseFn(parser: anytype, reader: anytype, allocator: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), ArrayList(ParserType(parser))) {
    const ResultType = ArrayList(ParserType(parser));
    const Result = ParseResult(@TypeOf(reader), ResultType);
    const SuccessResult = ParseResultSuccess(@TypeOf(reader), ResultType);

    var list = ArrayList(ParserType(parser)).init(allocator);

    var _reader = reader;
    while (true) {
        // Try to add to the list
        var arena = ArenaAllocator.init(allocator);
        const parseResult = try parser.parseWithAllocator(_reader, arena.allocator());
        if (parseResult == .success) {
            // Append and advance the reader
            try list.append(parseResult.success.result);
            _reader = parseResult.success.reader;
        } else {
            arena.deinit();
            // At the first failure, return the list as gathered so far
            // (even if it's empty)
            return Result{
                .success = SuccessResult{
                    .reader = _reader,
                    .result = list,
                },
            };
        }
    }
}

/// A zero-or-more parser greedily parses a value repeatedly, returning an arraylist of the successes
pub fn ZeroOrMoreParser(comptime parser: anytype) Parser(parser, ArrayList(ParserType(parser)), zeroOrMoreParseFn) {
    return Parser(parser, ArrayList(ParserType(parser)), zeroOrMoreParseFn){};
}

/// A function to parse a sequence of items, one-by-one
pub fn sequenceParseFn(parsers: anytype, reader: anytype, allocator: Allocator) ReaderLib.ReadError!ParseResult(@TypeOf(reader), ParsersType(parsers)) {
    const ResultType = ParsersType(parsers);
    const Result = ParseResult(@TypeOf(reader), ResultType);
    const SuccessResult = ParseResultSuccess(@TypeOf(reader), ResultType);

    var list: ResultType = undefined;

    var _reader = reader;
    var arena = ArenaAllocator.init(allocator);
    inline for (parsers) |parser, i| {
        // Try to add to the list
        const parseResult = try parser.parseWithAllocator(_reader, arena.allocator());
        if (parseResult == .success) {
            // Append and advance the reader
            list[i] = parseResult.success.result;
            _reader = parseResult.success.reader;
        } else {
            // Fail if they don't all succeed
            arena.deinit();
            return .fail;
        }
    }

    // Everything succeeded, return the result tuple
    return Result{
        .success = SuccessResult{
            .reader = _reader,
            .result = list,
        },
    };
}

/// A sequence parser tries to parse a sequence of values, returning only if they all pass
pub fn SequenceParser(comptime parsers: anytype) Parser(parsers, ParsersType(parsers), sequenceParseFn) {
    return Parser(parsers, ParsersType(parsers), sequenceParseFn){};
}

test "literal parsing" {
    var reader = ReaderLib.StringReader{ .context = ReaderLib.StringReaderContext{ .buffer = "hellothere" }, .allocator = testing.allocator };
    const helloParser = LiteralParser("hello");
    const thereParser = LiteralParser("there");

    try expect(try helloParser.parse(reader) == .success);
    try expect(try thereParser.parse(reader) == .fail);

    var parseResult = try helloParser.parse(reader);
    try expect(parseResult == .success);
    reader = parseResult.success.reader;

    try expect(try helloParser.parse(reader) == .fail);

    parseResult = try thereParser.parse(reader);
    try expect(parseResult == .success);
    reader = parseResult.success.reader;

    try expect(try helloParser.parse(reader) == .fail);
    try expect(try thereParser.parse(reader) == .fail);
}

test "one of parser" {
    var reader = ReaderLib.StringReader{ .context = ReaderLib.StringReaderContext{ .buffer = "hellothere" }, .allocator = testing.allocator };
    const HelloParser = LiteralParser("hello");
    const ThereParser = LiteralParser("there");
    const HelloOrThereParser = OneOfParser(.{ HelloParser, ThereParser });

    var parseResult = try HelloOrThereParser.parse(reader);
    try expect(parseResult == .success);
    try expect(eql(u8, parseResult.success.result, "hello"));

    parseResult = try HelloOrThereParser.parse(reader);
    try expect(parseResult == .success);
    try expect(eql(u8, parseResult.success.result, "hello"));

    // Update reader
    reader = parseResult.success.reader;

    parseResult = try HelloOrThereParser.parse(reader);
    try expect(parseResult == .success);
    try expect(eql(u8, parseResult.success.result, "there"));
    parseResult = try HelloOrThereParser.parse(reader);
    try expect(parseResult == .success);
    try expect(eql(u8, parseResult.success.result, "there"));

    // Update reader
    reader = parseResult.success.reader;
    try expect(try HelloOrThereParser.parse(reader) == .fail);
}

test "optional parser" {
    var reader = ReaderLib.StringReader{ .context = ReaderLib.StringReaderContext{ .buffer = "hellothere" }, .allocator = testing.allocator };
    const OptionalHelloParser = OptionalParser(LiteralParser("hello"));

    var parseResult = try OptionalHelloParser.parse(reader);
    try expect(parseResult == .success);
    try expect(eql(u8, parseResult.success.result.?, "hello"));

    // Update reader
    reader = parseResult.success.reader;

    // Even though it doesn't match, it won't fail
    parseResult = try OptionalHelloParser.parse(reader);
    try expect(parseResult == .success);
    try expect(parseResult.success.result == null);
    try expect(parseResult.success.reader.context.position == reader.context.position);
}

test "zero or more parser" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var reader = ReaderLib.StringReader{ .context = ReaderLib.StringReaderContext{ .buffer = "hellohellohello" }, .allocator = arena.allocator() };
    const ListHelloParser = ZeroOrMoreParser(LiteralParser("hello"));

    var parseResult = try ListHelloParser.parse(reader);
    try expect(parseResult == .success);
    try std.testing.expectEqual(parseResult.success.result.items.len, 3);
    try std.testing.expectEqualStrings(parseResult.success.result.items[0], "hello");
    try std.testing.expectEqualStrings(parseResult.success.result.items[1], "hello");
    try std.testing.expectEqualStrings(parseResult.success.result.items[2], "hello");

    // Update reader
    reader = parseResult.success.reader;

    parseResult = try ListHelloParser.parse(reader);
    try expect(parseResult == .success);
    try std.testing.expectEqual(parseResult.success.result.items.len, 0);
}

test "sequence parsing" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var reader = ReaderLib.StringReader{ .context = ReaderLib.StringReaderContext{ .buffer = "hellogoodbyehello" }, .allocator = arena.allocator() };

    const HelloParser = LiteralParser("hello");
    const GoodbyeParser = LiteralParser("goodbye");

    const HelloGoodbyeParser = SequenceParser(.{ HelloParser, GoodbyeParser });
    var parseResult = try HelloGoodbyeParser.parse(reader);
    try expect(parseResult == .success);
    try std.testing.expectEqual(parseResult.success.result.len, 2);
    try std.testing.expectEqualStrings(parseResult.success.result[0], "hello");
    try std.testing.expectEqualStrings(parseResult.success.result[1], "goodbye");

    // Update reader
    reader = parseResult.success.reader;

    // A subsequent parse should fail
    parseResult = try HelloGoodbyeParser.parse(reader);
    try expect(parseResult == .fail);

    // Ensure only the first "hello" and "goodbye" have been read.
    try expect(reader.context.position == 12);
}

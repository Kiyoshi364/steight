const std = @import("std");
const print = std.debug.print;
const ArenaAllocator = std.heap.ArenaAllocator;
const page_allocator = std.heap.page_allocator;

const lexer = @import("lexer.zig").lexer;
const sim = @import("sim.zig").sim;

pub fn main() anyerror!void {

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const lexerAlloc = &arena.allocator;

    const stdin = std.io.getStdIn();

    const size = 0xFF;
    var buffer: [size]u8 = undefined;
    print("reading: ", .{});

    // Ignoring <CR><LF>
    const trim = if (std.builtin.os.tag == .windows) 2 else 1;
    const read = (try stdin.read(&buffer)) - trim;
    const input = buffer[0..read];

    print("    input({}): {s}<LF>\n", .{ read, input });

    const ret = try lexer(lexerAlloc, input);

    print("    parsed: ({s}, {s})\n", .{ ret.value, ret.rest } );

    print("\n === simulation ===\n", .{} );
    sim(ret.value, .{ .verbose = false });
}

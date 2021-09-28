pub const InstEnum = enum {
    push,
    add,
    sub,
};

pub const Inst = union(InstEnum) {
    push: i64,
    add: void,
    sub: void,

    const std = @import("std");
    pub fn format(self: @This(), comptime fmt: []const u8,
            options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt; _ = options;
        switch (self) {
            .push => |val| try writer.print("(PUSH, {d})", .{ val }),
            .add => try writer.print("(ADD, {{}})", .{}),
            .sub => try writer.print("(SUB, {{}})", .{}),
        }
    }
};

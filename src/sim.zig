const std = @import("std");
const debug = std.debug;
const print = debug.print;
const assert = debug.assert;

const Inst = @import("Inst.zig").Inst;

pub const SimOpts = struct {
    verbose: bool = true,
    print_stack: bool = true,
};

const s_size = 0xFF;

pub fn sim(prog: []const Inst, opts: SimOpts) void {
    var stack: [s_size]i64 = undefined;
    var sp: usize = 0;
    var ip: usize = 0;
    while ( ip < prog.len ) : ( ip += 1 ) {
        const op = prog[ip];
        switch ( op ) {
            .push => |val| {
                checkStack(sp, 1) catch |err| switch (err) {
                    error.Overflow => {
                        print("Overflow on instruction number {}\n", .{ ip });
                        break;
                    },
                    else => unreachable,
                };
                if ( opts.verbose )
                    print("push({d}): {d}\n", .{ ip, val });
                stack[sp] = val;
                sp += 1;
            },
            .add => {
                checkStack(sp, -2) catch |err| switch (err) {
                    error.Underflow => {
                        print("Underflow on instruction number {}\n", .{ ip });
                        break;
                    },
                    else => unreachable,
                };
                const a = stack[sp-2];
                const b = stack[sp-1];
                const res = a + b;
                if ( opts.verbose )
                    print("add({d}): {d} <- {d} + {d}\n", .{ ip, res, a, b });
                sp -= 1;
                stack[sp-1] = res;
            },
            .sub => {
                checkStack(sp, -2) catch |err| switch (err) {
                    error.Underflow => {
                        print("Underflow on instruction number {}\n", .{ ip });
                        break;
                    },
                    else => unreachable,
                };
                const a = stack[sp-2];
                const b = stack[sp-1];
                const res = a - b;
                if ( opts.verbose )
                    print("sub({d}): {d} <- {d} + {d}\n", .{ ip, res, a, b });
                sp -= 1;
                stack[sp-1] = res;
            },
        }
    }
    if ( opts.print_stack ) printStack(&stack, sp);
    return;
}

fn printStack(stack: []const i64, sp: usize) void {
    if ( sp > 0 ) {
        print("stack({d}): ", .{ sp });
        for (stack) |val, i| {
            if ( i >= sp ) break;
            print("{d} ", .{ val });
        }
        print("\n", .{});
    } else {
        print("stack: <empty>\n", .{});
    }
}

fn checkStack(sp: usize, comptime n: comptime_int) !void {
    if ( n > 0 ) {
        if ( sp + n > s_size )
            return error.Overflow;
    } else {
        const isp = @intCast(isize, sp);
        if ( isp + n < 0 )
            return error.Underflow;
    }
    return;
}

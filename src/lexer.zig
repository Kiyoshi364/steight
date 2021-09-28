const mecha = @import("mecha/mecha.zig");

const Inst = @import("Inst.zig").Inst;

pub const lexer = mecha.many(
    token,
.{ .separator = whitespace });
const whitespace = mecha.many(mecha.ascii.space, .{});

const token = mecha.oneOf(.{
    number, builtin
});

const number = mecha.map(Inst,
    pushInt,
    mecha.int(i64, .{})
);
fn pushInt(x: i64) Inst { return .{ .push = x }; }

const builtin = mecha.oneOf(.{
    instPar("+"), instPar("-")
});

fn strToInst(comptime name: []const u8) fn(anytype) Inst {
    return struct { fn f(_:anytype) Inst {
        if ( comptime strEq(name, "+") ) {
            return .{ .add = .{} };
        } else if ( comptime strEq(name, "-") ) {
            return .{ .sub = .{} };
        } else {
            @compileLog(name, strEq(name, "+"), strEq(name, "-"));
            @compileError("Unexpected name '" ++ name ++ "' in strToInst");
        }
    } }.f;
}

fn instPar(comptime name: []const u8) mecha.Parser(Inst) {
    return mecha.map(Inst, strToInst(name), mecha.string(name));
}

fn strEq(comptime a: []const u8, comptime b: []const u8) bool {
    if ( a.len != b.len ) return false;
    for (a) |ai, i| {
        if ( ai != b[i] ) return false;
    } else return true;
}

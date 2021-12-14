//const input = @embedFile("../sample.txt");
const input = @embedFile("../input.txt");
const std = @import("std");

fn swap(c: u8) u8 {
    return switch (c) {
        ']' => '[',
        '>' => '<',
        ')' => '(',
        '}' => '{',
        else => unreachable,
    };
}

fn get_score(c: u8) usize {
    return switch (c) {
        ']' => 57,
        '>' => 25137,
        ')' => 3,
        '}' => 1197,
        else => unreachable,
    };
}

pub fn main() anyerror!void {
    var chunk_it = std.mem.tokenize(input, "\n");
    var result: usize = 0;
    while (chunk_it.next()) |chunk| {
        var stack = std.ArrayList(u8).init(std.heap.page_allocator);
        defer stack.deinit();
        for (chunk) |c| {
            switch (c) {
                '[', '(', '<', '{' => {
                    try stack.append(c);
                },
                else => {
                    if (stack.items.len == 0 or stack.items[stack.items.len - 1] != swap(c)) {
                        result += get_score(c);
                        break;
                    } else {
                        _ = stack.pop();
                    }
                },
            }
        }
    }
    std.log.info("{}", .{result});
}

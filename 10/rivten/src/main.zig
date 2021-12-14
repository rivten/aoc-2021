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

fn get_part_2_score(c: u8) usize {
    return switch (c) {
        '[' => 2,
        '<' => 4,
        '(' => 1,
        '{' => 3,
        else => unreachable,
    };
}
pub fn main() anyerror!void {
    var chunk_it = std.mem.tokenize(input, "\n");
    var result_part_1: usize = 0;
    var results_part_2 = std.ArrayList(usize).init(std.heap.page_allocator);
    while (chunk_it.next()) |chunk| {
        var stack = std.ArrayList(u8).init(std.heap.page_allocator);
        defer stack.deinit();
        var is_ok_line = true;
        for (chunk) |c| {
            switch (c) {
                '[', '(', '<', '{' => {
                    try stack.append(c);
                },
                else => {
                    if (stack.items.len == 0 or stack.items[stack.items.len - 1] != swap(c)) {
                        result_part_1 += get_score(c);
                        is_ok_line = false;
                        break;
                    } else {
                        _ = stack.pop();
                    }
                },
            }
        }
        if (is_ok_line) {
            var index: usize = 0;
            var result: usize = 0;
            while (index < stack.items.len) : (index += 1) {
                result *= 5;
                result += get_part_2_score(stack.items[stack.items.len - 1 - index]);
            }
            try results_part_2.append(result);
        }
    }
    std.log.info("{}", .{result_part_1});

    std.sort.sort(usize, results_part_2.items, {}, comptime std.sort.asc(usize));
    std.log.info("{}", .{results_part_2.items[(results_part_2.items.len - 1) / 2]});
}

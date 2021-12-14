//const input = @embedFile("../sample.txt");
const input = @embedFile("../input.txt");
const std = @import("std");

fn flood_fill(map: std.ArrayList([]u8), row_index: usize, col_index: usize) void {
    if (row_index >= map.items.len or col_index >= map.items[row_index].len) return;

    if (map.items[row_index][col_index] - '0' < 9) {
        map.items[row_index][col_index] = 255;
        if (row_index > 0) flood_fill(map, row_index - 1, col_index);
        flood_fill(map, row_index + 1, col_index);
        if (col_index > 0) flood_fill(map, row_index, col_index - 1);
        flood_fill(map, row_index, col_index + 1);
    }
}

fn compute_bassin_size_at_low_point(map: [][]const u8, row_index: usize, col_index: usize) !usize {
    var temp = std.ArrayList([]u8).init(std.heap.page_allocator);
    for (map) |line| try temp.append(try std.heap.page_allocator.dupe(u8, line));
    defer temp.deinit();
    flood_fill(temp, row_index, col_index);

    var result: usize = 0;
    for (temp.items) |line| {
        for (line) |c| {
            if (c == 255) {
                result += 1;
            }
        }
    }
    return result;
}

pub fn main() anyerror!void {
    var it = std.mem.tokenize(input, "\n");
    var lines = std.ArrayList([]const u8).init(std.heap.page_allocator);
    while (it.next()) |line| {
        try lines.append(line);
    }
    var bassin_sizes = std.ArrayList(usize).init(std.heap.page_allocator);
    var result: usize = 0;
    for (lines.items) |line, line_index| {
        for (line) |c, col_index| {
            var is_low_point = true;
            if (col_index > 0) {
                if (line[col_index - 1] <= c) {
                    is_low_point = false;
                }
            }
            if (col_index + 1 < line.len) {
                if (line[col_index + 1] <= c) {
                    is_low_point = false;
                }
            }
            if (line_index > 0) {
                if (lines.items[line_index - 1][col_index] <= c) {
                    is_low_point = false;
                }
            }
            if (line_index + 1 < lines.items.len) {
                if (lines.items[line_index + 1][col_index] <= c) {
                    is_low_point = false;
                }
            }
            if (is_low_point) {
                result += (c - '0') + 1;
                try bassin_sizes.append(try compute_bassin_size_at_low_point(lines.items, line_index, col_index));
            }
        }
    }

    std.sort.sort(usize, bassin_sizes.items, {}, comptime std.sort.desc(usize));
    std.log.info("{}", .{result});
    std.log.info("{}", .{bassin_sizes.items[0] * bassin_sizes.items[1] * bassin_sizes.items[2]});
}

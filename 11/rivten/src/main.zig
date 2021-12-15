//const input = @embedFile("../sample.txt");
const input = @embedFile("../input.txt");
const std = @import("std");

const line_count = comptime std.mem.count(u8, input, "\n");
const col_count = comptime std.mem.indexOfScalar(u8, input, '\n').?;

fn elem(flashed_octopus: std.ArrayList([2]usize), o: [2]usize) bool {
    for (flashed_octopus.items) |test_o| {
        if (test_o[0] == o[0] and test_o[1] == o[1]) {
            return true;
        }
    }
    return false;
}

fn doOneStep(octopuses: *[line_count][col_count]u8, flash_count: *usize) !void {
    for (octopuses) |*line| {
        for (line) |*energy| {
            energy.* += 1;
        }
    }
    var octopuses_to_flash = std.ArrayList([2]usize).init(std.heap.page_allocator);
    defer octopuses_to_flash.deinit();

    var flashed_octopus = std.ArrayList([2]usize).init(std.heap.page_allocator);
    defer flashed_octopus.deinit();

    for (octopuses) |line, line_index| {
        for (line) |energy, col_index| {
            if (energy > 9) {
                try octopuses_to_flash.append(.{ line_index, col_index });
            }
        }
    }

    while (octopuses_to_flash.items.len != 0) {
        const octopi_pos = octopuses_to_flash.pop();
        try flashed_octopus.append(octopi_pos);
        var neighbours = std.ArrayList([2]usize).init(std.heap.page_allocator);
        defer neighbours.deinit();

        if (octopi_pos[0] > 0) {
            if (octopi_pos[1] > 0) {
                try neighbours.append([_]usize{ octopi_pos[0] - 1, octopi_pos[1] - 1 });
            }
            if (octopi_pos[1] + 1 < octopuses[octopi_pos[0] - 1].len) {
                try neighbours.append([_]usize{ octopi_pos[0] - 1, octopi_pos[1] + 1 });
            }
            try neighbours.append([_]usize{ octopi_pos[0] - 1, octopi_pos[1] });
        }
        if (octopi_pos[0] + 1 < octopuses.len) {
            if (octopi_pos[1] > 0) {
                try neighbours.append([_]usize{ octopi_pos[0] + 1, octopi_pos[1] - 1 });
            }
            if (octopi_pos[1] + 1 < octopuses[octopi_pos[0] + 1].len) {
                try neighbours.append([_]usize{ octopi_pos[0] + 1, octopi_pos[1] + 1 });
            }
            try neighbours.append([_]usize{ octopi_pos[0] + 1, octopi_pos[1] });
        }

        if (octopi_pos[1] > 0) {
            try neighbours.append([_]usize{ octopi_pos[0], octopi_pos[1] - 1 });
        }
        if (octopi_pos[1] + 1 < octopuses[octopi_pos[0]].len) {
            try neighbours.append([_]usize{ octopi_pos[0], octopi_pos[1] + 1 });
        }

        for (neighbours.items) |p| {
            octopuses[p[0]][p[1]] += 1;
            if (octopuses[p[0]][p[1]] > 9 and !elem(flashed_octopus, p) and !elem(octopuses_to_flash, p)) {
                try octopuses_to_flash.append(p);
            }
        }
    }
    for (flashed_octopus.items) |flashed| {
        octopuses[flashed[0]][flashed[1]] = 0;
    }
    flash_count.* = flashed_octopus.items.len;
}

pub fn main() anyerror!void {
    var octopuses: [line_count][col_count]u8 = undefined;
    {
        var line_it = std.mem.tokenize(input, "\n");
        var line_index: usize = 0;
        while (line_it.next()) |line| {
            for (line) |c, col_index| {
                octopuses[line_index][col_index] = c - '0';
            }
            line_index += 1;
        }
    }

    var step_index: usize = 0;
    var flash_count: usize = 0;
    var all_flash_step = false;
    while (!all_flash_step) : (step_index += 1) {
        if (@mod(step_index, 100) == 0) std.debug.print("Doing step {}\n", .{step_index + 1});
        var step_flash_count: usize = 0;
        try doOneStep(&octopuses, &step_flash_count);
        flash_count += step_flash_count;
        all_flash_step = step_flash_count == col_count * line_count;

        //std.debug.print("\nAfter step {}:\n", .{step_index + 1});
        //for (octopuses) |line| {
        //    for (line) |energy| {
        //        std.debug.print("{}", .{energy});
        //    }
        //    std.debug.print("\n", .{});
        //}
    }
    //std.debug.print("{}\n", .{flash_count});
    std.debug.print("{}\n", .{step_index});
}

const input = @embedFile("../sample.txt");
//const input = @embedFile("../input.txt");
const std = @import("std");

const Vertex = [2]usize;

fn is_v_in_q(v: Vertex, q: std.ArrayList(Vertex)) bool {
    for (q.items) |test_v| {
        if (test_v[0] == v[0] and test_v[1] == v[1]) {
            return true;
        }
    }
    return false;
}

fn get_map_value(map: std.ArrayList([]const u8), line_index: usize, col_index: usize) usize {
    const v = map.items[@mod(line_index, map.items.len)][@mod(col_index, map.items[0].len)] - '0' + @divFloor(line_index, map.items.len) + @divFloor(col_index, map.items[0].len);
    if (v > 9) {
        return v - 9;
    } else {
        return v;
    }
}

pub fn main() anyerror!void {
    var map_ = std.ArrayList([]const u8).init(std.heap.page_allocator);
    var line_iterator = std.mem.tokenize(input, "\n");
    while (line_iterator.next()) |line| {
        try map_.append(line);
    }

    var q = std.ArrayList(Vertex).init(std.heap.page_allocator);
    var dist = std.AutoHashMap(Vertex, usize).init(std.heap.page_allocator);
    var prev = std.AutoHashMap(Vertex, Vertex).init(std.heap.page_allocator);

    var line_index: usize = 0;
    while (line_index < 5 * map_.items.len) : (line_index += 1) {
        var col_index: usize = 0;
        while (col_index < 5 * map_.items[@mod(line_index, map_.items.len)].len) : (col_index += 1) {
            try dist.put([2]usize{ line_index, col_index }, std.math.maxInt(usize));
            try prev.put([2]usize{ line_index, col_index }, undefined);
            try q.append([2]usize{ line_index, col_index });
        }
    }

    try dist.put([2]usize{ 0, 0 }, 0);

    while (q.items.len != 0) {
        if (@mod(q.items.len, 100) == 0) {
            std.log.info("Remaining {}", .{q.items.len});
        }
        var u: Vertex = undefined;
        var u_index: usize = undefined;
        var dist_u: usize = std.math.maxInt(usize);
        for (q.items) |v, v_index| {
            if (dist.get(v).? < dist_u) {
                dist_u = dist.get(v).?;
                u = v;
                u_index = v_index;
            }
        }
        _ = q.swapRemove(u_index);

        if (u[0] > 0) {
            var v = Vertex{ u[0] - 1, u[1] };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + get_map_value(map_, v[0], v[1]);
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
        if (u[0] + 1 < 5 * map_.items.len) {
            var v = Vertex{ u[0] + 1, u[1] };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + get_map_value(map_, v[0], v[1]);
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
        if (u[1] > 0) {
            var v = Vertex{ u[0], u[1] - 1 };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + get_map_value(map_, v[0], v[1]);
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }

        if (u[1] < 5 * map_.items[@mod(u[0], map_.items.len)].len) {
            var v = Vertex{ u[0], u[1] + 1 };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + get_map_value(map_, v[0], v[1]);
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
    }

    std.log.info("{}", .{dist.get(Vertex{ 5 * map_.items.len - 1, 5 * map_.items[map_.items.len - 1].len - 1 }).?});
}

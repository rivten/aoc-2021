//const input = @embedFile("../sample.txt");
const input = @embedFile("../input.txt");
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

pub fn main() anyerror!void {
    var map = std.ArrayList([]const u8).init(std.heap.page_allocator);
    var line_iterator = std.mem.tokenize(input, "\n");
    while (line_iterator.next()) |line| {
        try map.append(line);
    }

    var q = std.ArrayList(Vertex).init(std.heap.page_allocator);
    var dist = std.AutoHashMap(Vertex, usize).init(std.heap.page_allocator);
    var prev = std.AutoHashMap(Vertex, Vertex).init(std.heap.page_allocator);

    for (map.items) |line, line_index| {
        for (line) |_, col_index| {
            try dist.put([2]usize{ line_index, col_index }, std.math.maxInt(usize));
            try prev.put([2]usize{ line_index, col_index }, undefined);
            try q.append([2]usize{ line_index, col_index });
        }
    }

    try dist.put([2]usize{ 0, 0 }, 0);

    while (q.items.len != 0) {
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
                var alt = dist.get(u).? + map.items[v[0]][v[1]] - '0';
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
        if (u[0] + 1 < map.items.len) {
            var v = Vertex{ u[0] + 1, u[1] };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + map.items[v[0]][v[1]] - '0';
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
        if (u[1] > 0) {
            var v = Vertex{ u[0], u[1] - 1 };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + map.items[v[0]][v[1]] - '0';
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }

        if (u[1] < map.items[u[0]].len) {
            var v = Vertex{ u[0], u[1] + 1 };
            if (is_v_in_q(v, q)) {
                var alt = dist.get(u).? + map.items[v[0]][v[1]] - '0';
                if (alt < dist.get(v).?) {
                    try dist.put(v, alt);
                    try prev.put(v, u);
                }
            }
        }
    }

    std.log.info("{}", .{dist.get(Vertex{ map.items.len - 1, map.items[map.items.len - 1].len - 1 }).?});
}

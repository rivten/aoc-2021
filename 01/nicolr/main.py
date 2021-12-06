def get_increase_count(depth_list):
    increase_list = map(lambda x, y: x < y, depth_list[:-1], depth_list[1:])
    return sum(increase_list)

with open('input.txt') as f: lines = f.read().splitlines()
lines = list(map(int, lines))
print("Part 1:", get_increase_count(lines))

aggregated_lines = list(map(lambda x, y, z: x+y+z, lines[:-2], lines[1:-1], lines[2:]))
print("Part 2:", get_increase_count(aggregated_lines))

from collections import Counter, UserList

class Path(UserList):
	def __hash__(self):
		return hash("".join(self))


def build_cave_map(input_file):
	nodes_list = set().union(*[set(node.split("-")) for node in input_file])
	cave_map = {node: [] for node in nodes_list}
	for node in input_file:
		left, right = node.split("-")
		cave_map[left].append(right)
		cave_map[right].append(left)
	return cave_map, nodes_list


def get_all_paths(cave_map, nodes_list, max_visit=1):
	can_visit = {k: True for k in nodes_list}
	can_visit["start"] = False
	path_set = continue_path(Path(["start"]), cave_map, can_visit, max_visit)
	return path_set


def continue_path(path, cave_map, can_visit, max_visit):
	current_cave = path[-1]
	can_visit = update_can_visit(can_visit, path, max_visit)
	if current_cave == "end":
		return set([path])
	else:
		path_set = set()
		for cave in cave_map[current_cave]:
			if can_visit[cave]:
				path_set = path_set.union(continue_path(path + [cave], cave_map, can_visit.copy(), max_visit))
	return path_set

def update_can_visit(can_visit_dict, path, max_visit):
	can_visit_dict = can_visit_dict.copy()
	last_visited_cave = path[-1]
	visited_small_cave_list = list(filter(is_small_cave, path))
	if last_visited_cave in visited_small_cave_list and Counter(visited_small_cave_list).most_common()[0][1] >= max_visit:
		for cave in filter(lambda c: can_visit_dict[c], visited_small_cave_list):
			can_visit_dict[cave] = False
	return can_visit_dict

def is_small_cave(cave):
	return cave.islower() and cave not in ["start", "end"]

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()
	cave_map, nodes_list = build_cave_map(input_file)

	all_paths = get_all_paths(cave_map, nodes_list)
	print(f"Part1: {len(all_paths)}")

	all_paths = get_all_paths(cave_map, nodes_list, max_visit=2)
	print(f"Part2: {len(all_paths)}")

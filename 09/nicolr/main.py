import numpy as np
from functools import reduce

def get_low_points(array_map):
	max_height = array_map.max()
	left_neighbour = np.diff(array_map, n=1, axis=1, prepend = max_height + 1) < 0
	up_neighbour = np.diff(array_map, n=1, axis=0, prepend = max_height + 1) < 0
	right_neighbour = np.flip(np.diff(np.flip(array_map, axis=1), n=1, axis=1, prepend = max_height + 1) < 0, axis=1)
	down_neighbour = np.flip(np.diff(np.flip(array_map, axis=0), n=1, axis=0, prepend = max_height + 1) < 0, axis=0)
	mask = left_neighbour & up_neighbour & right_neighbour & down_neighbour
	return np.ma.array(array_map, mask=~mask)

def explore_all_map(array_map):
	def recursive_explore(i, j):
		array_map[i, j] = False
		basin_size = 1
		if i != 0 and array_map[i - 1, j]:
			basin_size += recursive_explore(i - 1, j)
		if j!= 0 and array_map[i, j -1]:
			basin_size += recursive_explore(i, j - 1)
		if (i < array_map.shape[0] - 1) and array_map[i+1, j]:
			basin_size += recursive_explore(i + 1, j)
		if (j< array_map.shape[1] - 1) and array_map[i, j+1]:
			basin_size += recursive_explore(i, j + 1)
		return basin_size

	array_map = array_map < 9
	basin_size_list = []
	keep_going = True
	while keep_going:
		if array_map.any():
			i, j = np.unravel_index(np.argmax(array_map), array_map.shape)
			basin_size_list.append(recursive_explore(i, j))
		else:
			keep_going = False
	return basin_size_list


if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()
	array_map = np.array([list(map(int, list(input_data))) for input_data in input_file])
	
	low_points = get_low_points(array_map)
	print(f"Part1: {(low_points + 1).sum()}")
	
	basin_size = explore_all_map(array_map)
	print(f"Part2: {reduce(lambda x, y: x* y, sorted(basin_size)[-3:])}")

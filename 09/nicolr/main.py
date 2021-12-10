import numpy as np

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()

	height_map = [list(map(int, list(input_data))) for input_data in input_file]
	array_map = np.array(height_map)
	max_height = array_map.max()
	left_neighbour = np.diff(array_map, n=1, axis=1, prepend = max_height + 1) < 0
	up_neighbour = np.diff(array_map, n=1, axis=0, prepend = max_height + 1) < 0
	right_neighbour = np.flip(np.diff(np.flip(array_map, axis=1), n=1, axis=1, prepend = max_height + 1) < 0, axis=1)
	down_neighbour = np.flip(np.diff(np.flip(array_map, axis=0), n=1, axis=0, prepend = max_height + 1) < 0, axis=0)
	mask = left_neighbour & up_neighbour & right_neighbour & down_neighbour
	print("Part1: {(np.ma.array(array_map, mask=~mask) + 1 ).sum()}")

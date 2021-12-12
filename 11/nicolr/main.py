import numpy as np

def step(array_map, n=1):
	flash_count = 0
	for _ in range(n):
		array_map = array_map + 1
		while (array_map >= 10).any():
			array_map = increase_energy_after_flash(array_map)
		flash_count += (array_map < 0).sum()
		array_map = np.where(array_map < 0, 0, array_map)
	return array_map, flash_count

def increase_energy_after_flash(array_map):
	flash_array = np.pad(array_map >= 10, pad_width=1, constant_values=False)
	array_map = np.where(array_map < 10, array_map, -1000)

	down = np.roll(flash_array, shift = 1, axis=0)[1:-1, 1:-1].astype(int)
	up = np.roll(flash_array, shift = -1, axis=0)[1:-1, 1:-1].astype(int)
	right = np.roll(flash_array, shift = 1, axis=1)[1:-1, 1:-1].astype(int)
	left = np.roll(flash_array, shift = -1, axis=1)[1:-1, 1:-1].astype(int)
	downright = np.roll(flash_array, shift = (1, 1), axis=(0,1))[1:-1, 1:-1].astype(int)
	upright = np.roll(flash_array, shift = (-1, 1), axis=(0,1))[1:-1, 1:-1].astype(int)
	upleft = np.roll(flash_array, shift = (-1, -1), axis=(0,1))[1:-1, 1:-1].astype(int)
	downleft = np.roll(flash_array, shift = (1, -1), axis=(0,1))[1:-1, 1:-1].astype(int)

	increase_value = down + up + right + left + downright + upright + upleft + downleft
	array_map += increase_value	
	return array_map

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()
	array_map = np.array([list(map(int, list(input_data))) for input_data in input_file])

	_, flash_count = step(array_map, n=100)
	print(f"Part1: {flash_count}")

	step_count = 1
	while True:
		array_map, flash_count = step(array_map)
		if flash_count == array_map.shape[0] * array_map.shape[1]:
			break
		step_count +=1
	print(f"Part2: {step_count}")

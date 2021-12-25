import numpy as np

class Grid:
	def __init__(self, input_grid):
		x_list, y_list = zip(*[map(int, coordinates.split(",")) for coordinates in input_grid])
		self.grid = np.zeros((max(y_list) + 1, max(x_list) + 1)).astype(bool)
		self.grid[y_list, x_list] = True

	def fold(self, folding_instructions):
		for instruction in folding_instructions:
			print(instruction)
			left, right = instruction.split("=")
			axis, folding_index = left[-1], int(right)
			self.grid = self.fold_(self.grid, axis=axis, folding_index=folding_index)

	@staticmethod
	def fold_(grid, axis, folding_index):
		print(grid.shape)
		if axis == "y":
			up_grid, down_grid = grid[:(folding_index)], np.flipud(grid[(folding_index + 1):])
			if up_grid.size < down_grid.size:
				up_grid = np.pad(up_grid, ((down_grid.shape[0] - up_grid.shape[0], 0), (0,0)), constant_values=False)
			elif up_grid.size > down_grid.size:
				down_grid = np.pad(down_grid, ((up_grid.shape[0] - down_grid.shape[0], 0), (0,0)), constant_values=False)
			return up_grid | down_grid
		else:
			return Grid.fold_(grid.T, axis="y", folding_index=folding_index).T

	def get_code(self):
		code = np.full(self.grid.shape, " ")
		code =  np.where(self.grid, "#", code)
		return code

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read()
	input_grid, folding_instructions = map(lambda x: x.splitlines(), input_file.split("\n\n"))
	grid_map = Grid(input_grid)
	
	grid_map.fold(folding_instructions[:1])
	print(f"Part1: {grid_map.grid.sum()}")

	grid_map.fold(folding_instructions[1:])
	np.savetxt("result.txt", grid_map.get_code(), delimiter="", fmt="%c", encoding="utf8")
	

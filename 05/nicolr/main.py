import numpy as np

class Line:
	def __init__(self, line):
		self.x1, self.y1 = line[0]
		self.x2, self.y2 = line[1]

	@property
	def min_x(self):
		return min([self.x1, self.x2])

	@property
	def max_x(self):
		return max([self.x1, self.x2])

	@property
	def min_y(self):
		return min([self.y1, self.y2])

	@property
	def max_y(self):
		return max([self.y1, self.y2])
	
	def is_vertical(self):
		return self.y1 == self.y2

	def is_horizontal(self):
		return self.x1 == self.x2

class GridMap:
	def __init__(self, lines_list, count_diagonals=False):
		left_point, right_point = zip(*lines_list)
		points = left_point + right_point
		x_point, y_point = zip(*points)
		assert all(map(lambda x: x >=0, x_point + y_point))
		x_max, y_max = max(x_point), max(y_point) 
		self.grid = np.zeros((x_max + 1, y_max+1))
		self.count_diagonals = count_diagonals


	def update(self, line):
		line= Line(line)
		if line.is_horizontal():
			self.grid[line.x1, line.min_y:(line.max_y + 1)] +=1
		elif line.is_vertical():
			self.grid[line.min_x:(line.max_x + 1), line.y1] +=1
		elif self.count_diagonals:
			assert line.max_x - line.min_x == line.max_y- line.min_y
			x_indices = range(line.min_x, line.max_x + 1)
			y_indices = range(line.min_y, line.max_y + 1)
			if not(
				(line.min_x == line.x1 and line.min_y == line.y1) 
				or (line.max_x == line.x1 and line.max_y == line.y1)
				):
				y_indices = list(reversed(y_indices))
			self.grid[x_indices, y_indices] +=1


	def count_dangerous_zone(self, threshold=2):
		return (self.grid >= 2).sum()

if __name__ =="__main__":
	with open('input.txt') as f:lines = f.read().splitlines()
	lines = [line.split(" -> ") for line in lines]
	lines = [[tuple(map(int, point.split(","))) for point in line] for line in lines]

	grid_map = GridMap(lines, count_diagonals=False)
	[grid_map.update(line) for line in lines]
	print(f"Part1: {grid_map.count_dangerous_zone()}")

	grid_map = GridMap(lines, count_diagonals=True)
	[grid_map.update(line) for line in lines]
	print(f"Part2: {grid_map.count_dangerous_zone()}")

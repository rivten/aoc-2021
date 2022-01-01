import re
import math
from itertools import product

def part1(target_zone):
	""" Works if the target zone is below the submarine (ie y < 0)"""
	y_min = abs(target_zone[2])
	return y_min * (y_min - 1) // 2

def part2(target_zone):
	"""Works if the target is in the right down zone (x > 0, y < 0)"""
	x_min, x_max, y_min, _ = target_zone
	valid_velocities = dict()
	for v in product(range(int(math.sqrt(x_min)), x_max + 1), range(y_min, abs(y_min - 1))):
		traj = get_trajectory(v, target_zone)
		if is_valid(target_zone, traj):
			valid_velocities[v] = traj
	return valid_velocities

def get_position(vx, vy, step):
	y = step * vy - step * (step - 1) / 2
	if step > vx + 1:
		x = vx * (vx + 1) / 2
	else:
		x = vx * step - step * (step - 1) / 2
	return x, y

def get_trajectory(v, target_zone):
	vx, vy = v
	traj = [(0,0)]
	n_step = 8
	while (traj[-1][-1] > target_zone[2]) & (traj[-1][0] < target_zone[1]):
		traj += [get_position(vx, vy, step=s) for s in range(len(traj), len(traj) + n_step)]
	return traj

def is_valid(target_position, trajectory):
	x_min, x_max, y_min, y_max = target_zone
	x_n, y_n = zip(*trajectory)
	valid_x = map(lambda x: x_min <=x <= x_max, x_n)
	valid_y = map(lambda y: y_min <=y <= y_max, y_n)
	valid_trajectory = any([x & y for x,y in zip(valid_x, valid_y)])
	return valid_trajectory

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().strip()
	target_zone = list(map(int, re.findall(r'-?\d+', input_file)))
	print(f"Part1: {part1(target_zone)}")
	print(f"Part2: {len(part2(target_zone))}")

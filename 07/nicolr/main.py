from itertools import product, groupby
import statistics 

def part2_fuel_consumption(pos1, pos2):
	n = abs(pos2 - pos1)
	return n * (n + 1) / 2

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read()
	input_data = list(map(int, input_file.split(",")))
	N_data = len(input_data)

	input_median = statistics.median(input_data)
	distance_to_median = sum(map(lambda x: abs(x-input_median), input_data))
	print(f"Part1: {int(distance_to_median)}")

	pair_distance = [(p[0], part2_fuel_consumption(*p)) for p in product(range(min(input_data), max(input_data) +1), input_data)]
	cost_by_position = [(key, sum(g for k, g in group)) for key, group in groupby(pair_distance, key=lambda x: x[0])]
	min_cost = min(cost_by_position, key=lambda x: x[1])
	print(f"Part2: {int(min_cost[1])}")

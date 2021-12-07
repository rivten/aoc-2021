from collections import Counter

import numpy as np

class LanternFish:
	def __init__(self, timer=8):
		self.timer = timer

	def update(self):
		if self.timer == 0:
			self.timer = 6
			return LanternFish()
		else:
			self.timer -= 1

class FishBank:
	def __init__(self, timer_list):
		initial_fish_bank = [LanternFish(timer=t) for t in timer_list]
		self.list_of_fish = initial_fish_bank

	@property
	def size(self):
		return len(self.list_of_fish)

	def update(self, n=1):
		for _ in range(n):
			new_fish = [fish.update() for fish in self.list_of_fish]
			self.list_of_fish += ([fish for fish in new_fish if fish is not None])

class BigFishBank:
	def __init__(self, timer_list):
		fish_counter = Counter([t for t in timer_list])
		print(fish_counter)
		self.fish_counter = np.zeros(9, dtype=int)
		self.fish_counter[list(fish_counter)] = list(fish_counter.values())
		print(self.fish_counter)

	@property
	def size(self):
		return self.fish_counter.sum()

	def update(self, n=1):
		for _ in range(n):
			n_new_fish = self.fish_counter[0]
			self.fish_counter = np.roll(self.fish_counter, -1)
			self.fish_counter[6] += n_new_fish

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read()
	input_data = list(map(int, input_file.split(",")))
	print(len(input_data))

	fish_bank = FishBank(input_data)
	fish_bank.update(n=80)
	print(f"Part1: {fish_bank.size}")

	# fish_bank = BigFishBank(input_data)
	# fish_bank.update(n=80)
	# print(f"Part1: {fish_bank.size}")

	fish_bank = BigFishBank(input_data)
	fish_bank.update(n=256)
	print(f"Part2: {fish_bank.size}")

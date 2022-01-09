from math import floor, ceil
from functools import reduce 
from itertools import combinations

verbose=False

def explode(snailfish, verbose=verbose):
	snailfish = list(snailfish)
	new_snailfish_number = []
	was_reduced = False

	last_digit_index = None 
	depth = 0
	while snailfish:
		if depth == 5:
			left = snailfish.pop(0)
			while snailfish[0].isdigit(): left += snailfish.pop(0)
			snailfish.pop(0)
			right = snailfish.pop(0)
			while snailfish[0].isdigit(): right += snailfish.pop(0)
			snailfish.pop(0)

			if last_digit_index:
				new_snailfish_number[last_digit_index] += int(left)
			new_snailfish_number[-1] = 0
			while snailfish:
				s = snailfish.pop(0)
				if s.isdigit():
					while snailfish[0].isdigit(): s +=snailfish.pop(0)
					new_snailfish_number.append(int(s) + int(right))
					break
				else:
					new_snailfish_number.append(s)
			new_snailfish_number += snailfish
			was_reduced = True
			break
		else:
			s = snailfish.pop(0)
			if s == "[":
				depth+=1
			elif s == "]":
				depth -= 1
			elif s.isdigit():
				last_digit_index = len(new_snailfish_number)
				while snailfish[0].isdigit(): s +=snailfish.pop(0)
				s = int(s)
			new_snailfish_number.append(s)

	new_snailfish = "".join(map(str, new_snailfish_number))
	if was_reduced and verbose:
		print("explo", new_snailfish)
	return new_snailfish, was_reduced

def split(snailfish, verbose=verbose):
	snailfish = list(snailfish)
	new_snailfish_number = []
	was_reduced = False

	while snailfish:
		s = snailfish.pop(0)
		if s.isdigit() and snailfish[0].isdigit():
			while snailfish[0].isdigit(): 
				s += snailfish.pop(0)
			new_snailfish_number += ['[',int(s) // 2 , ',', ceil(int(s) / 2) ,']']
			new_snailfish_number += snailfish
			was_reduced = True
			break
		else:
			new_snailfish_number.append(s)
	new_snailfish = "".join(map(str, new_snailfish_number))
	if was_reduced and verbose:
		print("split", new_snailfish)
	return new_snailfish, was_reduced


def add_snailfish(snailfish1, snailfish2):
	snailfish = '[' + snailfish1 + ',' +snailfish2 + ']'
	try_reduction = True
	if verbose: print("add", snailfish)
	while try_reduction:
		snailfish, was_reduced = explode(snailfish)
		if not was_reduced:
			snailfish, was_reduced = split(snailfish) 
		try_reduction = was_reduced
	return snailfish
	
def compute_magnitude(snailfish):
	if isinstance(snailfish, int):
		magnitude = snailfish
	else:
		magnitude = 3 * compute_magnitude(snailfish[0]) + 2 * compute_magnitude(snailfish[1])
	return magnitude

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().strip().splitlines()
	snailfish = reduce(add_snailfish, input_file)
	print(f"Part1: {compute_magnitude(eval(snailfish))}")

	pair_addition = map(lambda x: compute_magnitude(eval(add_snailfish(*x))), combinations(input_file, 2))
	print(f"Part1: {max(pair_addition)}")		

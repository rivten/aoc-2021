from collections import Counter

def create_insertion_dict(raw_insertion_instruction):
	insertion_list = raw_insertion_instruction.replace(" ", "").splitlines()
	insertion_list = map(lambda insertion: insertion.split("->"), insertion_list)
	insertion_dict = dict(insertion_list)
	return insertion_dict

def step(sequence, insertion_dict, n_step=1):
	for _ in range(n_step):
		new_sequence = ""
		for i in range(len(sequence)-1):
			left_char, right_char = sequence[i:i+2]
			new_sequence += left_char + insertion_dict[left_char+right_char]
		sequence = new_sequence + sequence[-1]
	return sequence

def fast_step(sequence, insertion_dict, n_step=1):
	pair_counter = Counter()
	for i in range(len(sequence)-1):
		pair_counter.update(["".join(sequence[i:i+2])])
	for _ in range(n_step):
		new_pair_counter = {pair: 0 for pair in insertion_dict}
		for pair, n in pair_counter.items():
			new_pair_counter[pair[0] + insertion_dict[pair]] += n
			new_pair_counter[insertion_dict[pair] + pair[1]] += n
		pair_counter = new_pair_counter
	return pair_counter

def get_letter_counter(start_sequence, pair_counter):
	all_letters = set("".join(pair_counter))
	letter_counter = {k: 0 for k in all_letters}
	for k, v in pair_counter.items():
		letter_counter[k[0]] += v
		letter_counter[k[1]] += v 
	letter_counter[start_sequence[0]] +=1
	letter_counter[start_sequence[-1]] +=1
	letter_counter = {k: v // 2 for k, v in letter_counter.items()}
	return letter_counter

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read()
	start_sequence, insertion_list = input_file.split("\n\n")
	insertion_dict = create_insertion_dict(insertion_list)
	
	sequence = step(start_sequence, insertion_dict, n_step=10)
	sequence_counter = Counter(sequence).most_common()
	most_common, least_common = sequence_counter[0][1], sequence_counter[-1][1]
	print(f"Part1: {most_common - least_common}")

	pair_counter = fast_step(start_sequence, insertion_dict, n_step=40)
	letter_counter = get_letter_counter(start_sequence, pair_counter)
	print(f"Part2: {max(letter_counter.values()) - min(letter_counter.values())}")

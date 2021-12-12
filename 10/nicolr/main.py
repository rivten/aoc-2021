from queue import LifoQueue


matching_characters = dict(map(tuple, ["()", "[]", "{}", "<>"]))
illegal_character_score = {
	")": 3, 
	"]": 57,
	"}": 1197,
	">": 25137
}

automplete_character_score  = {
	")": 1, 
	"]": 2,
	"}": 3,
	">": 4
}

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()

	syntax_error_score = []
	autocomplete_score = []
	for line in input_file:
		character_queue = LifoQueue()
		is_corrupted = False
		for c in line:
			if c in matching_characters.keys():
				character_queue.put(c)
			elif matching_characters[character_queue.get()] != c:
				syntax_error_score.append(illegal_character_score[c])
				is_corrupted = True
				break
		if not is_corrupted:
			score = 0
			while not character_queue.empty():
				score = score * 5 + automplete_character_score[matching_characters[character_queue.get()]]
			autocomplete_score.append(score)

	print(f"Part1: {sum(syntax_error_score)}")
	print(f"Part2: {sorted(autocomplete_score)[len(autocomplete_score) // 2]}")

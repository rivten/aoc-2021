class DisplayDecoder:
	def __init__(self, code_str):
		list_of_code = code_str.strip().split()
		self.digit_to_code = self._decode(list_of_code)
		self.code_to_digit = {''.join(sorted(list(v))):k for k,v in self.digit_to_code.items()}

	def _decode(self, list_of_code):
		code_by_length = {length: [] for length in range(2, 8)}
		for code_str in list_of_code: code_by_length[len(code_str)].append(set(code_str))

		digit_to_code = {
		1: (one := code_by_length[2].pop()),
		7: code_by_length[3].pop(),
		4: code_by_length[4].pop(),
		8: code_by_length[7].pop(),
		3: (three := next(filter(lambda c: one.issubset(c), code_by_length[5]))),
		9: (nine := next(filter(lambda c: three.issubset(c), code_by_length[6]))),
		5: (five := next(filter(lambda c: (c.issubset(nine)) and (c != three), code_by_length[5]))),
		2: next(filter(lambda c: (c != five) and (c != three), code_by_length[5])),
		6: (six := next(filter(lambda c: (five.issubset(c)) and (c != nine), code_by_length[6]))),
		0: next(filter(lambda c: (c != six) and (c != nine), code_by_length[6])),
		}

		return digit_to_code

	def decode_digit(self, digit_code):
		digit_code = "".join(sorted(digit_code))
		return str(self.code_to_digit[digit_code])

	def decode_display(self, list_of_code):
		str_number = "".join(self.decode_digit(d) for d in list_of_code.strip().split())
		return int(str_number)

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().splitlines()
	input_data = [line.split('|') for line in input_file]
	all_digits, display_data = zip(*input_data)

	result = [map(lambda digit_code: len(digit_code) in [2, 3, 4, 7], data.strip().split()) for data in display_data]
	result = sum([sum(data) for data in result])
	print(f"Part1: {result}")

	result = [DisplayDecoder(digits_code).decode_display(display) for digits_code, display in zip(all_digits, display_data)]
	print(f"Part2: {sum(result)}")

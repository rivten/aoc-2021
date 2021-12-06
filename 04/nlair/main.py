import numpy as np

class Board:
	def __init__(self, str_board):
		board_lines = list(map(lambda x: x.split(), str_board.split("\n")))
		self.board = np.array(board_lines, dtype=object).astype(int)
		self.board_state = np.full(self.board.shape, False, dtype=bool)
	
	def update_board(self, value):
		self.board_state[np.where(self.board == value)] = True

	def wins(self):
		return self.board_state.all(0).any() or self.board_state.all(1).any()

	def get_score(self, last_value):
		score = np.ma.array(self.board, mask=self.board_state)
		score = score.sum()
		return score * last_value


def get_first_to_win(boards_list, bingo_sequence):
	for i, value in enumerate(bingo_sequence):
		for board in boards_list:
			board.update_board(value)
			if board.wins():
				return board.get_score(value)


def get_last_to_win(boards_list, bingo_sequence):
	for value in bingo_sequence:
		[board.update_board(value) for board in boards_list]
		if len(boards_list) > 1:
			boards_list = list(filter(lambda x: not x.wins(), boards_list))
		elif boards_list[0].wins():
			return boards_list[0].get_score(value)

with open('input.txt') as f:input_file = f.read()
input_data = input_file.strip().split("\n\n")
bingo_sequence = list(map(int, input_data.pop(0).split(",")))

boards_list = [Board(b) for b in input_data]
print(f"Part1: {get_first_to_win(boards_list, bingo_sequence)}")

boards_list = [Board(b) for b in input_data]
print(f"Part2: {get_last_to_win(boards_list, bingo_sequence)}")

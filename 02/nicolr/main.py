def compute_final_position(instruction_list, initial_position, func):
    [func(line, position_summary=initial_position) for line in lines]
    return initial_position

def get_position_increase_part1(instruction, position_summary):
    instruction_type, instruction_value = instruction.split(" ")
    position_summary[instruction_type] += int(instruction_value)

def get_position_increase_part2(instruction, position_summary):
    instruction_type, instruction_value = instruction.split(" ")
    instruction_value = int(instruction_value)
    if instruction_type == "down":
        position_summary["aim"] += instruction_value
    elif instruction_type == "up":
        position_summary["aim"] -= instruction_value
    elif instruction_type == "forward":
        position_summary["forward"] += instruction_value
        position_summary["depth"] += position_summary["aim"] * instruction_value

with open('input.txt') as f: lines = f.read().splitlines()
final_position = compute_final_position(lines, dict(forward=0, down=0, up=0), get_position_increase_part1)
print(f"Part1: {final_position['forward'] * (final_position['down'] - final_position['up'])}")

final_position = compute_final_position(lines, dict(forward=0, depth=0, aim=0), get_position_increase_part2)
print(f"Part2: {final_position['forward'] * final_position['depth']}")

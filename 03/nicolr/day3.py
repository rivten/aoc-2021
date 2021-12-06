from collections import Counter

def get_most_frequent_digits(logs):
    lines = logs.copy()
    first_line = lines.pop()
    counter_dict = {i: Counter([d]) for i, d in enumerate(first_line)}
    for line in lines:
        [counter_dict[i].update([d]) for i,d in enumerate(line)]
    most_frequent_digits = [
        sorted(counter_.items(), key=lambda pair: (pair[1], pair[0]), reverse=True)[0][0] for counter_ in counter_dict.values()
        ]
    least_frequent_digits = [
        sorted(counter_.items(), key=lambda pair: (pair[1], pair[0]), reverse=False)[0][0] for counter_ in counter_dict.values()
        ]
    return most_frequent_digits, least_frequent_digits

def find_rating(logs, rating_type):
    """ For part 2"""
    candidates_lines = lines
    for i in range(1, len(lines[0])+1):
        search_string = get_search_string(candidates_lines, rating_type)
        search_substring = search_string[:i]
        candidates_lines = list(filter(lambda x: x.startswith(search_substring), candidates_lines))
        if len(candidates_lines) == 1:
            rating = candidates_lines[0]
            return int(rating, 2)

def get_search_string(candidates_lines, rating_type):
    most_frequent_digits, least_frequent_digits = get_most_frequent_digits(logs=candidates_lines)
    if rating_type == "oxygen":
        search_string = "".join(most_frequent_digits)
    elif rating_type == "co2":
        search_string = "".join(least_frequent_digits)
    else:
        raise NotImplementedError
    return search_string

with open('input.txt') as f:lines = f.read().splitlines()

most_frequent_digits, least_frequent_digits = get_most_frequent_digits(logs=lines)
power = int("".join(most_frequent_digits), 2) * int("".join(least_frequent_digits), 2)
print(f"Part1: {power}")

oxygen_rating = find_rating(logs=lines, rating_type="oxygen")
co2_rating = find_rating(logs=lines, rating_type="co2")
print(f"Part2: {oxygen_rating * co2_rating}")

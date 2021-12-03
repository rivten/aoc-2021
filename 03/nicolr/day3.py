from collections import Counter

if __name__ == "__main__":
    with open('input.txt') as f:
        lines = f.readlines()

    first_line = lines.pop()
    counter_dict = {i: Counter([d]) for i, d in enumerate(first_line)}
    for line in lines:
        [counter_dict[i].update([d]) for i,d in enumerate(first_line)]
    
    most_frequent = [counter_.most_common()[0][0] for counter_ in counter_dict.values()]
    least_frequent = [str(1 - int(d)) for d in most_frequent]
    
    power = int("".join(most_frequent), 2) * int("".join(least_frequent), 2)
    print(power)

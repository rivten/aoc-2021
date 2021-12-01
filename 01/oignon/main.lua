-- Day 01 - Part 1
file = io.open("input.txt", "r");
input = {}
 for line in file:lines() do
    table.insert(input, tonumber(line));
 end

i = 1
temp = 0
counter = 0

while i < #input do
  if input[i] > temp then
    counter = counter + 1
  end
  temp = input[i]
  i = i + 1
end

print(counter)
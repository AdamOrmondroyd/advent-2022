import ast

with open("input.txt", 'r') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    lines[i] = line.strip()

lines = [ast.literal_eval(line) for line in lines if line != ""]

sorted_counter = 0

def compare(left, right):
    print(left)
    print(right)
    print()

    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return 1
        elif left > right:
            return -1
        return 0

    if isinstance(left, int): return compare([left], right)
    if isinstance(right, int): return compare(left, [right])
            

    # both are lists

    for (a, b) in zip(left, right):
        result = compare(a, b)
        if result != 0: return result
    if len(left) < len(right):
        return 1
    if len(left) == len(right):
        return 0
    return -1
    
result = 0
i = 1
while lines:
    left = lines[0]
    right = lines[1]

    lines = lines[2:]
    
    if compare(left, right)==1: result += i

    # print(compare(left,right))
    i += 1
    print("-----------------------------")
print(result)



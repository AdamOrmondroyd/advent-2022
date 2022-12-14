import ast

with open("input.txt", 'r') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    lines[i] = line.strip()

lines = [ast.literal_eval(line) for line in lines if line != ""]

def compare(left, right):
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

lines_a = lines # save lines for next part
while lines_a:
    left = lines_a[0]
    right = lines_a[1]

    lines_a = lines_a[2:]
    
    if compare(left, right)==1: result += i

    i += 1
print(f"part a {result}")

# part II
# just need to see if each packet appears before or after each divider
two = 1
six = 2
two_divider = [[2]]
six_divider = [[6]]

for line in lines:
    if compare(line, two_divider) == 1:
        two += 1
        six += 1
    elif compare(line, six_divider) == 1:
        six += 1

print("-------------------------------------------------------------")
print(f"part b: {two*six}")



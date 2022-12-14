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

lines_a = lines
while lines_a:
    left = lines_a[0]
    right = lines_a[1]

    lines_a = lines_a[2:]
    
    if compare(left, right)==1: result += i

    print(compare(left,right))
    i += 1
    print("-----------------------------")
print(result)

# part II

lines.append([[2]])
lines.append([[6]])

# just do bubblesort 
for ii in range(len(lines), 1, -1):
    for i in range(ii-1):
        print(i)
        if compare(lines[i], lines[i+1]) == -1:
            lines[i], lines[i+1] = lines[i+1], lines[i]

print("-------------------------------------------------------------")
for i, line in enumerate(lines, start=1):
    print(line)
    if line == [[2]]: two = i
    if line == [[6]]: six = i
    
print("-------------------------------------------------------------")
print(two*six)



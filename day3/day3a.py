def priority(character):
    x = ord(character)
    if x > 96: return x-96
    return x-38


total = 0
with open("input.txt", 'r') as f:
    lines = f.readlines()
    for line in lines:
        line = line.strip()
        print(line)
        left = set(line[:len(line)//2])
        right = set(line[len(line)//2:])

        intersection = left.intersection(right)
        i = intersection.pop()
        print(i)
        print(priority(i))
        total += priority(i)
        
print(total)


        

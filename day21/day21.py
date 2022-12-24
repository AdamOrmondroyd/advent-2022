with open("input.txt", 'r') as f:
    lines = [line.strip().split() for line in f.readlines()]

# formatting preparation
for i, line in enumerate(lines):
    lines[i][0] = line[0][:-1]
    if len(line) == 2:
        lines[i][1] = int(line[1])


def calc(a, op, b):
    if op == '+': return a+b
    if op == '-': return a-b
    if op == '*': return a*b
    return a//b

if __name__ == "__main__":
    while lines:
        line = lines[0]
        lines = lines[1:]
        result = None
        if len(line) == 2:
            result = line[1]
        elif isinstance(line[1], int) and isinstance(line[3], int):
            result = calc(*line[1:])

        if not lines:
            break

        if result is not None and lines:
            for i, _ in enumerate(lines):
                if len(_) == 2: continue
                if lines[i][1]==line[0]:
                    lines[i][1] = result
                if lines[i][3] == line[0]:
                    lines[i][3] = result
        else:
            lines.append(line)

    print(result)


import numpy as np

supermod = 11 * 17 * 5 * 13 * 19 * 2 * 3 * 7

class Monkey:
    def __init__(self, idx, items, opcode, operand, test, t, f):
        self.idx = idx
        self.items = items
        if operand == 'old':
            if opcode == '+':
                self.operation = lambda old: 2 * old
            elif opcode == '*':
                self.operation = lambda old: old**2
            else:
                raise ValueError(f"opcode {opcode} not recognised")
        else:
            if opcode == '+':
                self.operation = lambda old: old + int(operand)
            elif opcode == '*':
                self.operation = lambda old: old * int(operand)
            else:
                raise ValueError(f"opcode {opcode} not recognised")

        self.test = test
        self.t = t
        self.f = f

        self.num_inspections = 0

    def inspect(self, monkey_list):
        # loop like this as items get thrown as the loop goes along
        #print(monkey.idx)
        if not self.items: return
        for i in range(len(self.items)):
            self.num_inspections += 1
            item = self.items[0]
            self.items = self.items[1:]
            #print(f"inspect item {item}")
            item = self.operation(item)
            # item = item // 3
            #print(f"item value now {item}")
            #print(f"does it divide by {self.test}")
            monkey_to_throw_to = self.t if (item % self.test) == 0 else self.f
            item = item % supermod
            #print(f"throw to {monkey_to_throw_to}")
            monkey_list[monkey_to_throw_to].items.append(item)
            

        

monkeys = []


with open("input.txt", 'r') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    lines[i] = line.split()
   
while lines:
    idx = int(lines[0][1][:-1])
    items = lines[1][2:]
    items[:-1] = [i[:-1] for i in items[:-1]]
    items = [int(i) for i in items]
    opcode = lines[2][-2]
    operand = lines[2][-1]
    test = int(lines[3][-1])
    t = int(lines[4][-1])
    f = int(lines[5][-1])

    monkeys.append(Monkey(idx, items, opcode, operand, test, t, f))

    lines = lines[6:]
    if lines: lines = lines[1:]

#for monkey in monkeys:
    #print(f"{monkey.idx}: {monkey.items}")

for _ in range(10000):
    print(_)
    for monkey in monkeys:
        monkey.inspect(monkeys)

    # for monkey in monkeys:
        # print(f"{monkey.idx}: {monkey.items}")

inspections = []
for monkey in monkeys:
    print(monkey.num_inspections)
    inspections.append(monkey.num_inspections)

i = np.argmax(inspections)
ii = np.argmax(np.concatenate((inspections[:i], [0], inspections[i+1:])))
print(inspections[i] * inspections[ii])



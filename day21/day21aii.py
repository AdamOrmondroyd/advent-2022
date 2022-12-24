from day21 import lines, calc

d = {}

d = {line[0]: line[1:] for line in lines}

print(d)

def ast(root):
    expression = d[root]
    if len(expression)==1: return expression[0]
    return calc(ast(expression[0]), expression[1], ast(expression[2]))

if __name__ == '__main__':
    print(ast('root'))

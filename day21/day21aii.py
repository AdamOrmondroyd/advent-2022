from day21 import lines, calc

d = {}

d = {line[0]: line[1:] for line in lines}

def ast(d, root):
    expression = d[root]
    if len(expression)==1: return expression[0]
    return calc(ast(d, expression[0]), expression[1], ast(d, expression[2]))


def invert_to_root(dinv, humn, root):
    # start at humn
    if humn in dinv.keys(): return 
    if humn == root: return
    for k, v in zip(list(dinv.keys()), list(dinv.values())):
        if humn in v:
            found=True
            dinv.pop(k)
            # exit()

            invert_to_root(dinv, k, root)
            dinv[humn] = invert_expression(k, v, humn)
            break
        

    
def invert_expression(root, expression, new_root):
    [a, op, b] = expression
    c = root
    if op == '+':
        if a == new_root:
            return [c,'-',b]
        return [c,'-',a]
    elif op == '-':
        if a == new_root:
            return [b,'+',c]
        return [a,'-',c]
    elif op == '*':
        if a == new_root:
            return [c,'/',b]
        return [c,'/',a]
    else: 
        if a == new_root:
            return [b,'*',c]
        return [c,'/',a]


if __name__ == '__main__':
    print(f"part a: {ast(d, 'root')}")
    human = d.pop('humn')
    try:
        root = ast(d, d['root'][0])
        root_name = d['root'][2]
    except KeyError:
        root = ast(d, d['root'][2])
        root_name = d['root'][0]


    dinv = d.copy()

    invert_to_root(dinv, "humn", root_name)
    dinv[root_name] = [root]
    print(f"part b: {ast(dinv, 'humn')}")

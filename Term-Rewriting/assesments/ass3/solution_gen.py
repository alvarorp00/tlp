low, high = 1, 5

posibilities = []

pre = 'search initial =>* '
post = ' .'

for i in range(1, high + 1):
    for j in range(1, i + 1):
        peg = f'peg({i}, {j})'
        empties = ''
        for k in range(1, high + 1):
            for l in range(1, k + 1):
                if k == i and l == j:
                    continue
                empties += f' empty({k}, {l})'
        posibilities.append(pre + peg + empties + post)
                

with open('temp.out', 'w') as dump:
    for posibility in posibilities:
        dump.write(posibility + '\n')
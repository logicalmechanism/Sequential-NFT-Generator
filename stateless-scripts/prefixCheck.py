everything = []
for i in range(1000):
    x = hex(i)[-2:]
    if "x" in x:
        x = x.replace("x", "0")
    if x not in everything:
        print(i, x)
        everything.append(x)
    else:
        print('break',i,x)
        break
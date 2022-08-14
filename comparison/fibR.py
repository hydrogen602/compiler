import time
import sys


def fibR(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibR(n-1) + fibR(n-2)


t = time.time()

for i in range(30, 40):
    print(i)
    print(fibR(i))

delta_t = time.time() - t
print(delta_t, file=sys.stderr)

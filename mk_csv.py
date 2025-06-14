import sys
from math import *

FILE = sys.argv[1]
IN = "results/" + FILE + ".txt"
OUT = "results/" + FILE + ".csv"


def worst_3lgn(n):
    if 1 <= n <= 4:
        return ceil(log2(n))
    return 3 * ceil(log2(n)) - 5


def best_3lgn(n):
    if n == 1:
        return 0
    return 2 * ceil(log2(n)) - 2


def worst_2lgn(n):
    if n == 1:
        return 0
    return 2 * ceil(log2(n + 1)) - 3


def best_2lgn(n):
    return log2(n + 1) - 1


def worst_1lgn(s):
    # return lambda n: 0 if n == 1 else (1 + 1/s) * log2(n) + s + 9
    return lambda n: 0 if n == 1 else floor((1 + 1 / s) * floor(log2(n))) + s + 9


BW = {
    "3lgn": worst_3lgn,  # best_3lgn),
    "2lgn": worst_2lgn,  # best_2lgn) }
    "1lgn-skip-2": worst_1lgn(2),
    "1lgn-skip-4": worst_1lgn(4),
    "1lgn-skip-8": worst_1lgn(8),
    "1lgn-skip-16": worst_1lgn(16),
}

WORST = BW[FILE]


with open(IN) as f, open(OUT, "w+") as g:
    r = f.read().split("\n")
    ls = []
    for row in r:
        s = row.split()
        if len(s) >= 13:
            n = int(s[9]) + 1
            if FILE[0] == "1":
                ls.append([str(n), s[12], str(WORST(n)), str(worst_2lgn(n))])
            else:
                ls.append([str(n), s[12], str(WORST(n))])
    for i in ls:
        g.write(",".join(i))
        g.write("\n")

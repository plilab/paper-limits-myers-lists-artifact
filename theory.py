from math import ceil, log2, floor
import matplotlib.pyplot as plt

fig, ax = plt.subplots()
ax.set_xscale("log")
ax.set_xlabel("Length of List")
ax.set_ylabel("Theoretical Max Path Length")


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
    return lambda n: 0 if n == 1 else floor((1 + 1 / s) * floor(log2(n))) + s + 9


xs, y3, y2, y12, y14, y18, y116 = [], [], [], [], [], [], []
for i in range(1, 100_000):
    xs.append(i)
    y3.append(worst_3lgn(i))
    y2.append(worst_2lgn(i))
    y12.append(worst_1lgn(2)(i))
    y14.append(worst_1lgn(4)(i))
    y18.append(worst_1lgn(8)(i))
    y116.append(worst_1lgn(16)(i))

COMPUTED = {
    "3lgn": "Basic",
    "2lgn": "Improved",
    "1lgn-skip-2": "Advanced ($\\sigma = 2$)",
    "1lgn-skip-4": "Advanced ($\\sigma = 4$)",
    "1lgn-skip-8": "Advanced ($\\sigma = 8$)",
    "1lgn-skip-16": "Advanced ($\\sigma = 16$)",
}

ax.plot(xs, y3, label=COMPUTED["3lgn"], color="tab:green")
ax.plot(xs, y2, label=COMPUTED["2lgn"], color="tab:red")
ax.plot(xs, y12, label=COMPUTED["1lgn-skip-2"], color="tab:purple")
ax.plot(xs, y14, label=COMPUTED["1lgn-skip-4"], color="tab:brown")
ax.plot(xs, y18, label=COMPUTED["1lgn-skip-8"], color="tab:olive")
ax.plot(xs, y116, label=COMPUTED["1lgn-skip-16"], color="tab:orange")

plt.legend(loc="upper left")
plt.gca().set_aspect(0.05)
plt.xlim(1, 100_000)
plt.ylim((0, 50))
plt.savefig(f"charts/theory.svg", pad_inches=0, bbox_inches="tight")

from math import ceil, log2
import matplotlib.pyplot as plt
import sys

fig, ax = plt.subplots()
ax.set_xscale("log")
ax.set_xlabel("Length of List")
ax.set_ylabel("Path Length")
filename = sys.argv[1]
points = []


def worst_3lgn(n):
    if 1 <= n <= 4:
        return ceil(log2(n))
    return 3 * ceil(log2(n)) - 5


xs, ys, zs, ws, js = [], [], [], [], []
for row in open("results/" + filename + ".csv").read().split("\n"):
    if not row:
        continue
    row = row.split(",")
    if filename[0] == "1":
        x, y, z, w = map(int, row)
        ws.append(w)
        js.append(worst_3lgn(x))
    else:
        x, y, z = int(row[0]), int(row[1]), int(row[2])
        if filename[0] == "2":
            js.append(worst_3lgn(x))
    xs.append(x)
    ys.append(y)
    zs.append(z)

COMPUTED = {
    "3lgn": "$\\lceil\\lg n\\rceil~\\text{if } 1 \\leq n \\leq 4, 3\\lceil\\lg n\\rceil - 5~\\text{otherwise}$",
    "2lgn": "$0~\\text{if } n=1, 2\\lceil\\lg (n + 1)\\rceil - 3~\\text{otherwise}$",
    "1lgn-skip-2": "$\\lfloor (1 + \\frac{1}{2}) \\lfloor\\lg n \\rfloor  \\rfloor + 11$",
    "1lgn-skip-4": "$\\lfloor (1 + \\frac{1}{4}) \\lfloor\\lg n \\rfloor  \\rfloor + 13$",
    "1lgn-skip-8": "$\\lfloor (1 + \\frac{1}{8}) \\lfloor\\lg n \\rfloor  \\rfloor + 17$",
    "1lgn-skip-16": "$\\lfloor (1 + \\frac{1}{16}) \\lfloor\\lg n \\rfloor  \\rfloor + 25$",
}

COLORS = {
    "3lgn": "tab:green",
    "2lgn": "tab:red",
    "1lgn-skip-2": "tab:purple",
    "1lgn-skip-4": "tab:brown",
    "1lgn-skip-8": "tab:olive",
    "1lgn-skip-16": "tab:orange",
}

LABELS = {
    "3lgn": "Theoretical (Basic)",
    "2lgn": "Theoretical (Improved)",
    "1lgn-skip-2": "Theoretical (Advanced ($\\sigma = 2$))",
    "1lgn-skip-4": "Theoretical (Advanced ($\\sigma = 4$))",
    "1lgn-skip-8": "Theoretical (Advanced ($\\sigma = 8$))",
    "1lgn-skip-16": "Theoretical (Advanced ($\\sigma = 16$))",
}

ax.plot(xs, ys, label="Actual", color="tab:blue")
ax.plot(xs, zs, label=LABELS[filename], color=COLORS[filename], linewidth=3.0)
if filename[0] == "1":
    ax.plot(
        xs, ws, color=COLORS["2lgn"], label="Theoretical (Improved)", linestyle="--"
    )
if filename[0] == "1" or filename[0] == "2":
    ax.plot(xs, js, color=COLORS["3lgn"], label="Theoretical (Basic)", linestyle="--")

plt.xlim(1, 100_000)
plt.ylim((0, 50))
plt.legend(loc="upper left")
plt.savefig(f"charts/{filename}.svg", bbox_inches="tight", pad_inches=0)

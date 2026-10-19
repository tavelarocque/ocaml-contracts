"""The README's charts, drawn from the CSVs that viz/viz_data.ml writes.

    dune exec viz/viz_data.exe -- /tmp/viz      # the numbers, from the OCaml library
    python3 tools/plots.py /tmp/viz docs/img    # the pictures

Synthetic inputs only (the test suite's smile and simulated market), so nothing here uses real market data.
Needs matplotlib.
"""
import csv
import os
import sys
from collections import defaultdict

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402

SRC, OUT = sys.argv[1], sys.argv[2]
os.makedirs(OUT, exist_ok=True)

INK, MUTED, GRID = "#1f2328", "#59636e", "#e4e7eb"
BLUE, ORANGE, TEAL, RED = "#2b6cb0", "#dd6b20", "#2c7a7b", "#c53030"
plt.rcParams.update({
    "font.family": "DejaVu Sans", "font.size": 10, "axes.edgecolor": MUTED, "axes.labelcolor": INK,
    "xtick.color": MUTED, "ytick.color": MUTED, "axes.spines.top": False, "axes.spines.right": False,
    "axes.grid": True, "grid.color": GRID, "grid.linewidth": 0.8, "axes.titleweight": "bold",
    "axes.titlesize": 11, "axes.titlecolor": INK, "legend.frameon": False, "figure.dpi": 100,
})


def rows(name):
    with open(os.path.join(SRC, name), newline="") as f:
        return list(csv.DictReader(f))


def save(fig, name):
    fig.savefig(os.path.join(OUT, name), dpi=200, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print("wrote", os.path.join(OUT, name))


def note(ax, text):
    ax.text(0, -0.2, text, transform=ax.transAxes, fontsize=8, color=MUTED, va="top")


# 1. Hero: a digital's delta by strike and hours to close, a 3D surface.
d = rows("surf_digital.csv")
hours = sorted({float(r["hours"]) for r in d})
strikes = sorted({float(r["strike"]) for r in d})
Z = np.zeros((len(hours), len(strikes)))
for r in d:
    Z[hours.index(float(r["hours"])), strikes.index(float(r["strike"]))] = float(r["delta_cents_per_100"])
X, Y = np.meshgrid(np.array(strikes) / 1000, hours)
fig = plt.figure(figsize=(9, 5.2))
ax = fig.add_subplot(projection="3d")
ax.plot_surface(X, Y, Z, cmap="viridis", rstride=1, cstride=1, linewidth=0.2, edgecolor=(1, 1, 1, 0.25),
                antialiased=True)
ax.view_init(elev=28, azim=-58)
ax.set_xlabel("strike ($k)", labelpad=8)
ax.set_ylabel("hours to close", labelpad=8)
ax.set_zlabel("delta (¢ per $100)", labelpad=6)
ax.set_title('Delta of "BTC above K": a low hill a day out, a spike at the forward near the close', pad=4)
for a in (ax.xaxis, ax.yaxis, ax.zaxis):
    a.pane.set_facecolor((1, 1, 1, 0))
    a._axinfo["grid"]["color"] = GRID
fig.text(0.5, 0.02, "Synthetic one-day smile at a $100,000 forward, shortened to the hours left. "
         "Computed by the library (viz/viz_data.ml).", ha="center", fontsize=8, color=MUTED)
save(fig, "delta_surface.png")

# 2. Density under three distributions.
dist = rows("distribution.csv")
names = {"smile": ("SVI smile", BLUE, "-"), "lognormal": ("lognormal at ATM vol", ORANGE, "--"),
         "smile x0.80": ("smile narrowed to 80%", TEAL, "-")}
fig, ax = plt.subplots(figsize=(7.5, 3.6))
for m, (label, c, ls) in names.items():
    pts = [(float(r["strike"]) / 1000, 100 * float(r["density_per_1000"])) for r in dist if r["model"] == m]
    ax.plot(*zip(*pts), color=c, ls=ls, lw=2, label=label)
ax.set_xlabel("BTC price at expiry ($k)")
ax.set_ylabel("probability per $1,000 (%)")
ax.set_title("One smile, three distributions, one interface")
ax.legend()
note(ax, "Breeden–Litzenberger density from a synthetic one-day SVI smile; the same contracts price under each.")
save(fig, "density.png")

# 3. Pin risk.
hp = rows("hedge_paths.csv")
t = np.array([float(r["terminal"]) for r in hp]) / 1000
p = np.array([float(r["pnl_cents"]) for r in hp])
fig, ax = plt.subplots(figsize=(7.5, 3.6))
ax.scatter(t, p, s=4, alpha=0.35, color=BLUE, linewidths=0)
ax.axhline(0, color=MUTED, lw=0.8, ls="--")
ax.axvline(100, color=RED, lw=1, alpha=0.6)
ax.text(100.15, ax.get_ylim()[1] * 0.85, "strike", color=RED, fontsize=8)
ax.set_xlabel("where BTC finished ($k)")
ax.set_ylabel("seller's P&L (¢)")
ax.set_title("Pin risk: a hedged digital's error piles up at its strike")
note(ax, "4,000 paths on a 200-step Rubinstein implied tree; sold at-the-money digital, re-hedged every 16 steps.")
save(fig, "pin_risk.png")

# 4. Market making: P&L by alpha for each look-ahead.
mm = rows("mm_alpha.csv")
fig, ax = plt.subplots(figsize=(7.5, 3.6))
for la, c in zip(["1", "4", "10"], [TEAL, ORANGE, RED]):
    r = [x for x in mm if x["lookahead_steps"] == la]
    a = np.array([float(x["alpha"]) for x in r])
    m = np.array([float(x["pnl_mean"]) for x in r])
    se = np.array([float(x["pnl_se"]) for x in r])
    ax.plot(a, m, "o-", color=c, lw=2, ms=4, label=f"informed see {la} step{'s' if la != '1' else ''} ahead")
    ax.fill_between(a, m - 1.96 * se, m + 1.96 * se, color=c, alpha=0.15, lw=0)
ax.axhline(0, color=MUTED, lw=0.8, ls="--")
ax.set_xlabel("spread widening α (sd of the fair value's next move)")
ax.set_ylabel("P&L per day ($)")
ax.set_title("Avellaneda–Stoikov on a \\$0/\\$1 contract: widening can't beat traders who see far enough ahead")
ax.title.set_fontsize(10)
ax.legend(loc="lower right")
note(ax, "2,000 simulated days per point, 95% intervals. Against a 10-step look-ahead every spread loses and not quoting wins (Glosten–Milgrom).")
save(fig, "market_maker.png")

# 5. Markouts.
mk = rows("markouts.csv")
fig, ax = plt.subplots(figsize=(7.5, 3.6))
for share, c in zip(["0%", "50%", "90%"], [TEAL, ORANGE, RED]):
    r = [x for x in mk if x["informed_share"] == share]
    s = np.arange(len(r))
    m = np.array([float(x["mean_cents"]) for x in r])
    lo = np.array([float(x["ci_low_cents"]) for x in r])
    hi = np.array([float(x["ci_high_cents"]) for x in r])
    ax.plot(s, m, "o-", color=c, lw=2, ms=4, label=f"{share} of takers informed")
    ax.fill_between(s, lo, hi, color=c, alpha=0.15, lw=0)
ax.axhline(0, color=MUTED, lw=0.8, ls="--")
ax.set_xticks(range(6))
ax.set_xticklabels(["at the trade", "10s", "30s", "1m", "2m", "5m"])
ax.set_xlabel("time after the trade")
ax.set_ylabel("passive-side markout (¢)")
ax.set_title("Markouts tell informed flow from uninformed without knowing who anyone is")
ax.legend(loc="lower left")
note(ax, "Simulated 12-hour market, 2¢ half-spread, informed takers see a minute ahead; 95% intervals resample whole events.")
save(fig, "markouts.png")

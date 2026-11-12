"""Figure 7: conceptual summary of NESE foraging results (schematic, not data-driven).

Writes figures/summary/Fig7_summary.svg. Render to PDF/PNG with any SVG tool
(e.g. Inkscape, rsvg-convert, or a browser). Numbers in the text come from the
knitted reports of 10 Nov 2026; update them if the analyses are re-run.
"""
import math
import os
import random

random.seed(7)

W, H = 1800, 1180
SA, NCP, CC, MALE, GREY = "#1b9e77", "#d95f02", "#7570b3", "#d73027", "#7f7f7f"
INK, MUTED, RULE = "#222222", "#555555", "#bdbdbd"
FONT = "Arial, Helvetica, 'Liberation Sans', 'DejaVu Sans', sans-serif"

out = []
add = out.append


def text(x, y, s, size=20, weight="normal", fill=INK, anchor="start", style="normal"):
    add(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" font-weight="{weight}" '
        f'font-style="{style}" fill="{fill}" text-anchor="{anchor}">{s}</text>')


def lines(x, y, rows, size=19, gap=27, fill=INK, anchor="start"):
    for i, s in enumerate(rows):
        text(x, y + i * gap, s, size=size, fill=fill, anchor=anchor)


def cloud(cx, cy, sx, sy, n, col, r=6, rot=0.0):
    c, s = math.cos(rot), math.sin(rot)
    for _ in range(n):
        u, v = random.gauss(0, sx), random.gauss(0, sy)
        x, y = cx + u * c - v * s, cy + u * s + v * c
        add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{col}" fill-opacity="0.75" '
            f'stroke="white" stroke-width="1"/>')


def ellipse(cx, cy, rx, ry, col, rot=0, fill_op=0.18, dash=None, sw=3):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    add(f'<ellipse cx="{cx}" cy="{cy}" rx="{rx}" ry="{ry}" transform="rotate({rot} {cx} {cy})" '
        f'fill="{col}" fill-opacity="{fill_op}" stroke="{col}" stroke-width="{sw}"{d}/>')


def panel(x, y, w, h, letter, title):
    add(f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="10" fill="none" stroke="{RULE}" stroke-width="1.5"/>')
    text(x + 22, y + 42, letter, size=30, weight="bold")
    text(x + 60, y + 42, title, size=25, weight="bold")


add(f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
    f'font-family="{FONT}">')
add('<defs><marker id="arr" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" '
    'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#444"/></marker></defs>')
add(f'<rect width="{W}" height="{H}" fill="white"/>')

# ---------------------------------------------------------------- annual cycle
X0, X1, TY = 60, 1740, 70
mw = (X1 - X0) / 12
text(X0, 42, "Adult female annual cycle", size=22, weight="bold")
for i, m in enumerate("JFMAMJJASOND"):
    text(X0 + (i + 0.5) * mw, TY + 92, m, size=17, fill=MUTED, anchor="middle")
    add(f'<line x1="{X0 + i * mw:.1f}" y1="{TY + 66}" x2="{X0 + i * mw:.1f}" y2="{TY + 72}" stroke="{RULE}"/>')
add(f'<line x1="{X0}" y1="{TY + 66}" x2="{X1}" y2="{TY + 66}" stroke="{RULE}"/>')

day = mw * 12 / 365
segs = [  # start day, length (days), label, fill, text colour
    (0, 50, "Breeding (ashore)", "#e0e0e0", INK),
    (50, 74, "Post-breeding trip · 74 d", "#4d4d4d", "white"),
    (124, 30, "Moult", "#e0e0e0", INK),
    (154, 211, "Post-moult trip · 221 d (gestation)", "#9e9e9e", "white"),
]
for start, length, lab, f, tc in segs:
    xs, w = X0 + start * day, length * day
    add(f'<rect x="{xs:.1f}" y="{TY}" width="{w:.1f}" height="56" rx="6" fill="{f}" stroke="white" stroke-width="2"/>')
    text(xs + w / 2, TY + 35, lab, size=18 if w > 120 else 15, weight="bold", fill=tc, anchor="middle")

# Connectors from the trips to panels a and b
add(f'<path d="M{X0 + 87 * day:.1f},{TY + 100} L{X0 + 87 * day:.1f},{TY + 135} L460,{TY + 135} L460,{TY + 158}" '
    f'fill="none" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')
add(f'<path d="M{X0 + 260 * day:.1f},{TY + 100} L{X0 + 260 * day:.1f},{TY + 135} L1340,{TY + 135} L1340,{TY + 158}" '
    f'fill="none" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')

# ---------------------------------------------------------------- panel a: PB
PY, PH = 235, 430
panel(40, PY, 845, PH, "a", "Short trip: three discrete strategies")
# behaviour space
bx, by = 70, PY + 70
add(f'<line x1="{bx + 10}" y1="{by + 330}" x2="{bx + 470}" y2="{by + 330}" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')
add(f'<line x1="{bx + 10}" y1="{by + 330}" x2="{bx + 10}" y2="{by + 10}" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')
text(bx + 240, by + 352, "Offshore, deeper dives →", size=16, fill=MUTED, anchor="middle")
add(f'<text x="{bx - 8}" y="{by + 170}" font-size="16" fill="{MUTED}" text-anchor="middle" '
    f'transform="rotate(-90 {bx - 8} {by + 170})">Shelf use, benthic dives →</text>')
cloud(bx + 150, by + 210, 36, 30, 48, SA)
cloud(bx + 360, by + 240, 36, 30, 39, NCP)
cloud(bx + 95, by + 70, 14, 12, 6, CC, r=7)
text(bx + 150, by + 285, "Subarctic", size=18, weight="bold", fill=SA, anchor="middle")
text(bx + 360, by + 308, "N Central Pacific", size=18, weight="bold", fill=NCP, anchor="middle")
text(bx + 125, by + 60, "California Current", size=18, weight="bold", fill=CC)
tx = 560
lines(tx, PY + 100, ["Robust to analytical", "choices; stable groups",
                     "(Jaccard 0.85–0.99)"], size=18)
lines(tx, PY + 200, ["California Current:", "coastal, benthic, shorter",
                     "trips, faster mass gain"], size=18)
lines(tx, PY + 300, ["Offshore strategies: separate", "areas, same isotopic niche",
                     "(Δδ¹⁵N 0.40 ‰, negligible)"], size=18)

# ---------------------------------------------------------------- panel b: PM
panel(915, PY, 845, PH, "b", "Long trip: a continuum")
gx0, gy0 = 960, PY + 300
# continuous band of points coloured from Subarctic to N Central Pacific
def mix(c1, c2, t):
    a = [int(c1[i:i + 2], 16) for i in (1, 3, 5)]
    b = [int(c2[i:i + 2], 16) for i in (1, 3, 5)]
    return "#" + "".join(f"{round(a[k] + (b[k] - a[k]) * t):02x}" for k in range(3))
for _ in range(86):
    t = min(max(random.gauss(0.5, 0.22), 0), 1)
    x = gx0 + 20 + t * 430
    y = gy0 - 40 + random.gauss(0, 34) - 60 * math.sin(t * math.pi) * 0.4
    add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="6" fill="{mix(SA, NCP, t)}" fill-opacity="0.8" '
        f'stroke="white" stroke-width="1"/>')
# density outline: one peak
pts = []
for i in range(61):
    t = i / 60
    pts.append(f"{gx0 + 20 + t * 430:.1f},{gy0 + 70 - 75 * math.exp(-((t - 0.5) ** 2) / (2 * 0.2 ** 2)):.1f}")
add(f'<polyline points="{" ".join(pts)}" fill="none" stroke="#444" stroke-width="2.5"/>')
add(f'<line x1="{gx0 + 20}" y1="{gy0 + 70}" x2="{gx0 + 450}" y2="{gy0 + 70}" stroke="#444" stroke-width="1.5"/>')
text(gx0 + 235, gy0 + 96, "High latitude  ⟷  low latitude", size=16, fill=MUTED, anchor="middle")
text(gx0 + 235, PY + 95, "One peak, no gap (dip test P = 0.81)", size=17, fill=MUTED, anchor="middle")
lines(1445, PY + 100, ["No discrete groups;", "no coastal strategy"], size=18)
lines(1445, PY + 180, ["Isotopes follow each", "female’s position on", "the gradient, not group",
                       "(ΔAICc ≈ 30–38)"], size=18)
lines(1445, PY + 312, ["More California", "Current use →", "higher δ¹³C, δ¹⁵N"], size=18)

# ---------------------------------------------------------------- panel c: consistency
QY, QH = 695, 450
panel(40, QY, 845, QH, "c", "Females: consistent in where, not how")
col_x, col_y = 360, QY + 395  # colony
add(f'<path d="M{col_x - 60},{QY + 425} Q{col_x + 40},{QY + 360} {col_x + 120},{QY + 425}" fill="none" stroke="#9e9e9e" stroke-width="2"/>')
text(col_x + 40, QY + 438, "coast", size=14, fill=MUTED, anchor="middle")
add(f'<circle cx="{col_x}" cy="{col_y}" r="8" fill="#222"/>')
text(col_x + 14, col_y + 6, "colony", size=15, fill=MUTED)
# female A (Subarctic): two trips to the same area (north)
for dx, op in ((0, 1), (18, 0.75)):
    add(f'<path d="M{col_x},{col_y} C{col_x - 40 + dx},{QY + 270} {col_x - 10 + dx},{QY + 130} {col_x + 60 + dx},{QY + 105} '
        f'C{col_x + 120 + dx},{QY + 90} {col_x + 70 + dx},{QY + 250} {col_x},{col_y}" fill="none" stroke="{SA}" '
        f'stroke-width="3" stroke-opacity="{op}"/>')
ellipse(col_x + 70, QY + 125, 55, 32, SA, 0, 0.25, sw=2)
# female B (N Central Pacific): two trips offshore
for dy, op in ((0, 1), (16, 0.75)):
    add(f'<path d="M{col_x},{col_y} C{col_x - 120},{QY + 330 + dy} {col_x - 220},{QY + 230 + dy} {col_x - 245},{QY + 200 + dy} '
        f'C{col_x - 270},{QY + 160 + dy} {col_x - 150},{QY + 260 + dy} {col_x},{col_y}" fill="none" stroke="{NCP}" '
        f'stroke-width="3" stroke-opacity="{op}"/>')
ellipse(col_x - 240, QY + 205, 50, 30, NCP, 0, 0.25, sw=2)
text(col_x + 70, QY + 82, "Female A, two trips", size=15, fill=SA, anchor="middle")
text(col_x - 240, QY + 160, "Female B, two trips", size=15, fill=NCP, anchor="middle")
lines(560, QY + 100, ["Same strategy in 23 of", "27 trip pairs; changes", "only between trip types"], size=18)
lines(560, QY + 200, ["Return to own area", "(overlap 0.81 vs 0.46", "same-strategy females)"], size=18)
lines(560, QY + 300, ["Isotopes repeatable", "(R ≈ 0.76); diving", "effort not (R = 0.26)"], size=18)

# ---------------------------------------------------------------- panel d: males
panel(915, QY, 845, QH, "d", "Males: one broad, enriched niche")
ox, oy, s = 990, QY + 400, 125   # origin and px per permil
add(f'<line x1="{ox}" y1="{oy}" x2="{ox + 420}" y2="{oy}" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')
add(f'<line x1="{ox}" y1="{oy}" x2="{ox}" y2="{oy - 320}" stroke="#444" stroke-width="2" marker-end="url(#arr)"/>')
text(ox + 210, oy + 30, "δ¹³C", size=18, fill=MUTED, anchor="middle")
add(f'<text x="{ox - 22}" y="{oy - 160}" font-size="18" fill="{MUTED}" text-anchor="middle" '
    f'transform="rotate(-90 {ox - 22} {oy - 160})">δ¹⁵N</text>')
fx, fy = ox + 110, oy - 105   # offshore females' centre
ellipse(fx, fy, 58, 30, SA, -25)
ellipse(fx + 0.07 * s, fy - 0.40 * s * 0.6, 52, 26, NCP, -25)
ellipse(fx + 0.59 * s, fy - 1.16 * s * 0.6, 50, 36, CC, -25, dash="6 4")
ellipse(fx + 1.40 * s, fy - 1.20 * s * 0.6, 112, 50, MALE, -38, 0.12, sw=3.5)
text(fx + 1.40 * s + 20, fy - 1.2 * s * 0.6 + 94, "Males", size=19, weight="bold", fill=MALE, anchor="middle")
text(fx - 55, fy + 55, "Offshore females", size=16, fill=MUTED)
text(fx + 0.59 * s - 50, fy - 1.16 * s * 0.6 - 32, "California Current", size=16, fill=CC, anchor="end")
lines(1445, QY + 100, ["No discrete groups;", "one isotopic gradient"], size=18)
lines(1445, QY + 180, ["Niche 2–4× wider", "than any female group"], size=18)
lines(1445, QY + 260, ["Most females fall in", "the male niche; few", "males in female niches"], size=18)
text(1445, QY + 370, "Schematic; not to scale", size=15, fill=MUTED, style="italic")

add("</svg>")

os.makedirs("../figures/summary", exist_ok=True)
with open("../figures/summary/Fig7_summary.svg", "w", encoding="utf-8") as f:
    f.write("\n".join(out))
print("wrote ../figures/summary/Fig7_summary.svg")

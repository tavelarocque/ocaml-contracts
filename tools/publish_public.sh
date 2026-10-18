#!/bin/bash
# Publish a clean public snapshot of this repository.
#
#   bash tools/publish_public.sh                       dry run: build the public copy in a temp folder, test it, stop
#   bash tools/publish_public.sh https://github.com/tavelarocque/ocaml-contracts.git      ...and push it there
#
# The private repository stays where the work happens. The public copy differs in four ways:
#   1. No history: it starts from a fresh commit of the current committed tree (HEAD), and each later publish
#      adds one commit on top. Older private commits mention results of Implied Odds' layer 5 test, which is
#      sealed until 2026-11-30.
#   2. The test fixture's real Kalshi quotes (bid, ask and their sizes, from the sealed window) are blanked.
#      The tests only use its strikes, smiles and reference probabilities.
#   3. Commits are authored with a GitHub no-reply address, never the private one. Set PUBLIC_EMAIL to the
#      address shown at github.com/settings/emails (the one with your numeric ID links commits to your profile).
#   4. README_PUBLIC.md becomes its README (the private one is a working notebook), with a short note at the end
#      saying all of this.
# Before anything is pushed it checks the copy for the private email and for any Kalshi quotes in any CSV, then
# builds it and runs every test. Any failure stops it.
set -euo pipefail

remote="${1:-}"
public_email="${PUBLIC_EMAIL:-tavelarocque@users.noreply.github.com}"
public_name="${PUBLIC_NAME:-Tave La Rocque}"
root="$(git rev-parse --show-toplevel)"
cd "$root"

if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "Uncommitted changes: commit first, so the public copy matches a known commit." >&2
  exit 1
fi
rev="$(git rev-parse --short HEAD)"
tmp="$(mktemp -d "${TMPDIR:-/tmp}/ocaml-contracts.XXXXXX")"
tree="$tmp/tree"
mkdir -p "$tree"
git archive HEAD | tar -x -C "$tree"
echo "Public copy of $rev in $tree"

# 2. Blank the fixture's quotes, by column name.
python3 - "$tree/test/data/contracts_sample.csv" <<'PY'
import csv, sys
path = sys.argv[1]
rows = list(csv.reader(open(path, newline="")))
head = rows[0]
blank = [head.index(c) for c in ("bid", "ask", "bid_size", "ask_size")]
for r in rows[1:]:
    for i in blank:
        r[i] = ""
csv.writer(open(path, "w", newline=""), lineterminator="\n").writerows(rows)
print(f"  blanked bid, ask and sizes on {len(rows) - 1} fixture rows")
PY

# The public landing page: README_PUBLIC.md replaces the private working README.
if [ -f "$tree/README_PUBLIC.md" ]; then
  mv "$tree/README_PUBLIC.md" "$tree/README.md"
  echo "  README_PUBLIC.md is the public README"
fi

# 4. Say so in the README.
cat >> "$tree/README.md" <<EOF

## About this repository

This is a public snapshot of a private working repository, published with \`tools/publish_public.sh\` (from commit
$rev). History starts fresh here, and the Kalshi quotes in \`test/data/contracts_sample.csv\` are blank: they fall
inside the window of a pre-registered test in the companion research project, Implied Odds, which is sealed until
2026-11-30. The tests use only that file's strikes, smiles and reference probabilities, so they run unchanged.
EOF

# Checks: nothing private, nothing sealed.
fail=0
# The private address is read from this repository's own commits, so it is never written in this file.
private_email="$(git log -1 --format=%ae)"
private_user="${private_email%@*}"
if [ "$private_email" != "$public_email" ] && grep -rIl -e "$private_email" -e "$private_user" "$tree" >/dev/null; then
  echo "STOP: the private email appears in:" >&2; grep -rIl -e "$private_email" -e "$private_user" "$tree" >&2; fail=1
fi
# Sealed results: the figures from the one look before the seal was known were removed from every file at the
# seal commit, and all history before it is left out (point 1), so none can reach the public copy. They are not
# listed here, since this script is published too. What could still slip in is new data: the next check.
python3 - "$tree" <<'PY' || fail=1
import csv, glob, os, sys
bad = []
for path in glob.glob(os.path.join(sys.argv[1], "**", "*.csv"), recursive=True):
    rows = list(csv.reader(open(path, newline="")))
    if not rows:
        continue
    cols = [i for i, c in enumerate(rows[0]) if c in ("bid", "ask")]
    if any(r[i] for r in rows[1:] for i in cols if i < len(r)):
        bad.append(path)
if bad:
    print("STOP: Kalshi quotes left in: " + ", ".join(bad), file=sys.stderr)
    sys.exit(1)
print("  no quotes left in any CSV")
PY
[ "$fail" = 0 ] || exit 1
echo "  no private email"

# Build and test the public copy exactly as a visitor would.
(cd "$tree" && dune build 2>&1 | grep -v '^\[WARNING\]' || true)
(cd "$tree" && dune build >/dev/null 2>&1) || { echo "STOP: the public copy does not build" >&2; exit 1; }
if ! (cd "$tree" && dune runtest >"$tmp/test.log" 2>&1); then
  echo "STOP: tests fail in the public copy; see $tmp/test.log" >&2
  exit 1
fi
rm -rf "$tree/_build"
echo "  builds, and every test passes"

if [ -z "$remote" ]; then
  echo "Dry run done. Look around: $tree"
  echo "To publish: bash tools/publish_public.sh <public repo URL>"
  exit 0
fi

# Publish: add one commit on top of whatever the public repo has.
pub="$tmp/pub"
if git ls-remote --exit-code "$remote" HEAD >/dev/null 2>&1; then
  git clone --quiet "$remote" "$pub"
  find "$pub" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
else
  mkdir -p "$pub" && git -C "$pub" init --quiet -b main
  git -C "$pub" remote add origin "$remote"
fi
cp -R "$tree/." "$pub/"
git -C "$pub" add -A
if git -C "$pub" diff --cached --quiet; then
  echo "Nothing changed since the last publish."
  exit 0
fi
git -C "$pub" -c user.name="$public_name" -c user.email="$public_email" \
  commit --quiet -m "Snapshot of the private repository at $rev"
git -C "$pub" push --quiet origin HEAD:main
echo "Published $rev to $remote as $public_email"

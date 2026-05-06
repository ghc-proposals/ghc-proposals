"""Calibrate classify_vote against the historical Google Sheet labels.

The shepherd Google Sheet has hand-labeled per-member votes for ~50 PRs.
By aligning those labels with the corresponding messages in the cached
mbox archive, we get a labeled corpus to test classify_vote against.

Run after shepherd_dashboard.py has been used at least once with mbox
fetching enabled (so ~/.cache/shepherd_dashboard/ has data):

    python3 shepherd_dashboard.py --ml-months 60   # populate the cache
    python3 tune_classifier.py                     # report metrics
"""

import csv
import io
import sys
import urllib.request
from collections import Counter, defaultdict

import shepherd_dashboard as sd


SHEET_URL = (
    "https://docs.google.com/spreadsheets/d/"
    "1e6GdwHmAjeDEUhTvP-b18MDkpTfH3SMHhFu5F3nDIWc/gviz/tq?tqx=out:csv"
)

# Spreadsheet vote columns appear in this order. Names match the canonical
# entries in shepherd_dashboard.COMMITTEE_MEMBERS.
SHEET_VOTE_MEMBERS = [
    "Simon Marlow",
    "Simon Peyton Jones",
    "Malte Ott",
    "Matthías Páll Gissurarson",
    "Eric Seidel",
    "Adam Gundry",
    "Jaro Reinders",
    "Jeff Young",
    "Rodrigo Mesquita",
    "Jakob Brünker",
    "Sebastian Graf",
    "Erik de Castro Lopo",
    "Moritz Angermann",
]

# Column indices (0-based) within a row
COL_PR = 0
COL_TITLE = 1
COL_VOTES_START = 9


def normalize_label(raw):
    """Normalize a free-text vote cell to one of accept/reject/recuse/concern,
    or None to drop (too ambiguous to use as ground truth)."""
    s = (raw or "").lower().strip()
    if not s:
        return None
    # Drop ranking-style multi-vote cells ("A = B > C > D")
    if any(c in s for c in "=<>"):
        return None
    # Administrative annotations in vote-like cells
    if s.startswith(("accepted ", "rejected ", "sent for revision", "12 feb", "author closes")):
        return None

    # Canonical buckets
    if s in ("recuse", "recuses", "abstain", "abstains", "co-author", "author"):
        return "recuse"
    if s.startswith(("accept", "approve", "acc ", "no obj", "fine", "happy", "agree", "lgtm")):
        return "accept"
    if s in ("+1",) or s.startswith("+1 "):
        return "accept"
    if s.startswith(("reject", "oppose", "block", "decline")):
        return "reject"
    if s == "-1" or s.startswith("-1 "):
        return "reject"
    if s.startswith(("concern", "worry", "reservation", "hesit", "unconvinced")):
        return "concern"
    if s.startswith("amended"):
        # Amendment voting style — not a typical vote signal, drop
        return None
    if s in ("a", "b", "c", "d"):
        # Single-letter ranking entries
        return None
    return None


def fetch_spreadsheet():
    """Download the GHC Steering Committee spreadsheet as CSV text."""
    req = urllib.request.Request(SHEET_URL, headers={"User-Agent": "tune-classifier"})
    with urllib.request.urlopen(req) as resp:
        return resp.read().decode("utf-8")


def parse_spreadsheet(csv_text):
    """Yield (pr_number, member_canonical, normalized_vote) triples."""
    reader = csv.reader(io.StringIO(csv_text))
    rows = list(reader)
    triples = []
    raw_distribution = Counter()
    for row in rows:
        if not row or len(row) <= COL_PR:
            continue
        cell0 = row[COL_PR].strip()
        if not cell0.startswith("#"):
            continue  # section headers like "Closed", "Accepted"
        try:
            pr = int(cell0.lstrip("#").rstrip(":"))
        except ValueError:
            continue
        for i, member in enumerate(SHEET_VOTE_MEMBERS):
            col = COL_VOTES_START + i
            if col >= len(row):
                break
            raw = row[col].strip()
            if raw:
                raw_distribution[raw] += 1
            label = normalize_label(raw)
            if label:
                triples.append((pr, member, label))
    return triples, raw_distribution


def build_corpus(triples, messages):
    """Pair each (pr, member) label with the FIRST message that member sent in
    threads about that PR. Returns (corpus, unaligned)."""
    by_msgid = {m["message_id"]: m for m in messages if m["message_id"]}

    def pr_refs_for(msg, _seen=None):
        if _seen is None:
            _seen = set()
        if msg["message_id"] in _seen:
            return []
        _seen.add(msg["message_id"])
        if msg["pr_refs"]:
            return msg["pr_refs"]
        parent = by_msgid.get(msg["in_reply_to"])
        if parent:
            return pr_refs_for(parent, _seen)
        for ref in msg["references"]:
            parent = by_msgid.get(ref)
            if parent and parent["pr_refs"]:
                return parent["pr_refs"]
        return []

    by_pr = defaultdict(list)
    for m in messages:
        for pr in pr_refs_for(m):
            by_pr[pr].append(m)

    corpus = []
    unaligned = []
    for pr, member, label in triples:
        candidates = [
            m for m in by_pr.get(pr, [])
            if sd.match_committee_member(m["from_name"]) == member
        ]
        if not candidates:
            unaligned.append((pr, member, label))
            continue
        candidates.sort(key=lambda m: m["datetime"])
        first = candidates[0]
        body = first["clean_body"] or first["body"]
        corpus.append((pr, member, label, body))
    return corpus, unaligned


CLASSES = ("accept", "reject", "recuse", "concern", "unclear")


def confusion_matrix(corpus):
    matrix = {true: Counter() for true in CLASSES}
    for pr, member, true_label, body in corpus:
        pred, _ = sd.classify_vote(body)
        matrix[true_label][pred] += 1
    return matrix


def report(matrix, total):
    print("=" * 78)
    print(f"Calibration corpus: {total} (PR, member) pairs with ground-truth labels.")
    print()
    print("Confusion matrix (rows = true, cols = predicted):")
    head = "true\\pred ".ljust(12) + "".join(c.ljust(10) for c in CLASSES) + "total"
    print(head)
    print("-" * len(head))
    for true in CLASSES:
        row = matrix[true]
        total_row = sum(row.values())
        print(true.ljust(12) + "".join(str(row.get(c, 0)).ljust(10) for c in CLASSES) + str(total_row))
    print()
    print("Per-class precision / recall (excluding 'unclear' as a target class):")
    for c in CLASSES:
        if c == "unclear":
            continue
        tp = matrix[c].get(c, 0)
        fp = sum(matrix[t].get(c, 0) for t in CLASSES if t != c)
        fn = sum(matrix[c].get(p, 0) for p in CLASSES if p != c)
        p = tp / (tp + fp) if (tp + fp) else 0.0
        r = tp / (tp + fn) if (tp + fn) else 0.0
        marker = "OK " if (p >= 0.85 and r >= 0.70) else "   "
        print(f"  {marker}{c:8s}  P={p:6.1%}  R={r:6.1%}  (tp={tp:3d} fp={fp:3d} fn={fn:3d})")
    print()


def show_errors(corpus, max_per_bucket=4):
    by_bucket = defaultdict(list)
    for pr, member, true_label, body in corpus:
        pred, _ = sd.classify_vote(body)
        if pred != true_label:
            by_bucket[(true_label, pred)].append((pr, member, body))
    print("Top errors by (true → predicted):")
    # Largest buckets first
    sorted_buckets = sorted(by_bucket.items(), key=lambda kv: -len(kv[1]))
    for (true, pred), items in sorted_buckets:
        print(f"\n  {true} → {pred}  ({len(items)} cases)")
        for pr, member, body in items[:max_per_bucket]:
            snippet = body[:300].replace("\n", " ")
            print(f"    #{pr} {member}: {snippet!r}")
    print()


def show_distribution(triples, raw_distribution):
    counts = Counter(t[2] for t in triples)
    print("Label distribution (after normalization):")
    for c in CLASSES:
        print(f"  {c:10s}  {counts.get(c, 0)}")
    print()
    # Show raw labels we DIDN'T normalize (helps spot label-normalisation bugs)
    unmapped = sorted(
        ((raw, n) for raw, n in raw_distribution.items() if normalize_label(raw) is None and n >= 2),
        key=lambda kv: -kv[1],
    )[:20]
    if unmapped:
        print("Raw-label values dropped (top 20 by frequency, ≥2 occurrences):")
        for raw, n in unmapped:
            print(f"  {n:3d}× {raw!r}")
        print()


def main():
    print("Fetching spreadsheet...", file=sys.stderr)
    csv_text = fetch_spreadsheet()
    triples, raw_distribution = parse_spreadsheet(csv_text)
    print(f"  {len(triples)} (PR, member, vote) labels", file=sys.stderr)

    print("Loading mbox cache...", file=sys.stderr)
    cache = sd.cache_dir()
    if not cache.exists():
        print(
            "ERROR: no mbox cache. Run shepherd_dashboard.py with --ml-months >= 24 first.",
            file=sys.stderr,
        )
        sys.exit(1)
    paths = sorted(cache.glob("*.mbox"))
    if not paths:
        print(
            "ERROR: cache exists but contains no .mbox files. Re-run shepherd_dashboard.py.",
            file=sys.stderr,
        )
        sys.exit(1)
    print(f"  {len(paths)} cached mbox files", file=sys.stderr)
    messages = sd.parse_archive(paths)
    print(f"  {len(messages)} messages parsed", file=sys.stderr)

    print("Building corpus...", file=sys.stderr)
    corpus, unaligned = build_corpus(triples, messages)
    print(f"  {len(corpus)} aligned (label, body) pairs", file=sys.stderr)
    print(f"  {len(unaligned)} unaligned (no message found for that member on that PR)", file=sys.stderr)
    print()

    show_distribution(triples, raw_distribution)
    matrix = confusion_matrix(corpus)
    report(matrix, len(corpus))
    show_errors(corpus)


if __name__ == "__main__":
    main()

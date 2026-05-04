#!/usr/bin/env python3
"""
GHC Proposals Shepherd Dashboard

Zero-dependency script that aggregates proposal status from:
  1. GitHub Search API (PR metadata, labels, dates)
  2. HyperKitty mailing list archive (shepherd assignments, discussion activity)

Usage:
  python3 shepherd_dashboard.py [OPTIONS]

Options:
  --closed-months N    Include closed/merged PRs from last N months (default: open only)
  --format FORMAT      Output: ascii (default), csv, html
  --output FILE        Output file (default: stdout for ascii/csv, proposals.html for html)
  --github-token TOKEN Override GITHUB_TOKEN env var
  --ml-months N        Months of mailing list to scrape (default: 6)
  --no-ml              Skip mailing list scraping
"""

import argparse
import csv
import io
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone
from html.parser import HTMLParser


# ---------------------------------------------------------------------------
# GitHub
# ---------------------------------------------------------------------------

GITHUB_SEARCH_URL = "https://api.github.com/search/issues"
REPO = "ghc-proposals/ghc-proposals"
EXCLUDE_LABELS = ["Non-proposal", "No proposal needed", "Out-of-scope"]

LABEL_STATUS_MAP = {
    "Pending shepherd recommendation": "Pending Shepherd",
    "Pending committee review": "Pending Committee",
    "Needs revision": "Needs Revision",
    "Accepted": "Accepted",
    "Rejected": "Rejected",
    "Dormant": "Dormant",
}


def github_headers(token=None):
    headers = {"Accept": "application/vnd.github.v3+json", "User-Agent": "ghc-proposals-dashboard"}
    if token:
        headers["Authorization"] = f"token {token}"
    return headers


def github_search(query, token=None):
    """Run a GitHub search query, paginating through all results."""
    items = []
    page = 1
    while True:
        params = urllib.parse.urlencode({"q": query, "per_page": 100, "page": page})
        url = f"{GITHUB_SEARCH_URL}?{params}"
        req = urllib.request.Request(url, headers=github_headers(token))
        try:
            with urllib.request.urlopen(req) as resp:
                data = json.loads(resp.read().decode())
        except urllib.error.HTTPError as e:
            if e.code == 403:
                print(f"Warning: GitHub rate limit hit. Got {len(items)} results so far.", file=sys.stderr)
                break
            raise
        items.extend(data.get("items", []))
        if len(items) >= data.get("total_count", 0):
            break
        page += 1
        time.sleep(1)  # be nice to the search API
    return items


def fetch_proposals(token=None, closed_months=None):
    """Fetch proposals from GitHub."""
    exclude = " ".join(f'-label:"{l}"' for l in EXCLUDE_LABELS)
    base_q = f"repo:{REPO} is:pr {exclude}"

    proposals = {}

    # Open PRs
    print("Fetching open PRs from GitHub...", file=sys.stderr)
    for item in github_search(f"{base_q} is:open", token):
        pr = parse_github_item(item)
        proposals[pr["number"]] = pr

    # Closed PRs if requested
    if closed_months:
        cutoff = (datetime.now(timezone.utc) - timedelta(days=closed_months * 30)).strftime("%Y-%m-%d")
        print(f"Fetching closed PRs since {cutoff}...", file=sys.stderr)
        # Unauthenticated search API: 10 req/min. Wait to avoid rate limit.
        time.sleep(7)
        for item in github_search(f"{base_q} is:closed closed:>{cutoff}", token):
            pr = parse_github_item(item)
            if pr["number"] not in proposals:
                proposals[pr["number"]] = pr

    return proposals


def parse_github_item(item):
    labels = [l["name"] for l in item.get("labels", [])]
    status = "Open"
    for label, mapped in LABEL_STATUS_MAP.items():
        if label in labels:
            status = mapped
            break
    if item.get("pull_request", {}).get("merged_at"):
        status = "Merged"

    is_amendment = "Amendment" in labels

    return {
        "number": item["number"],
        "title": item["title"],
        "author": item["user"]["login"],
        "status": status,
        "is_amendment": is_amendment,
        "created_at": item["created_at"][:10],
        "updated_at": item["updated_at"][:10],
        "gh_url": item["html_url"],
        # mailing list fields filled later
        "shepherd": "",
        "ml_threads": 0,
        "ml_last_activity": "",
        "ml_participants": 0,
    }


# ---------------------------------------------------------------------------
# Mailing List (HyperKitty)
# ---------------------------------------------------------------------------

ML_BASE = "https://mailman.haskell.org/archives/list/ghc-steering-committee@haskell.org"

# Patterns for shepherd assignment thread subjects
SHEPHERD_SUBJECT_PATTERNS = [
    re.compile(r"Assigning\s+Shepherd.*?#(\d+)", re.IGNORECASE),
    re.compile(r"Please\s+review\s+#(\d+)", re.IGNORECASE),
    re.compile(r"Shepherd.*?#(\d+)", re.IGNORECASE),
]

# Patterns for shepherd name in email body
SHEPHERD_BODY_PATTERNS = [
    re.compile(r"nominate\s+\*?([^*\n]+?)\*?\s+as\s+(?:the\s+)?shepherd", re.IGNORECASE),
    re.compile(r"shepherd\s+(?:will be|is|:)\s+\*?([^*\n,]+?)\*?\s*[.,\n]", re.IGNORECASE),
]

# Pattern for PR number references in subjects
PR_REF_PATTERN = re.compile(r"#(\d+)")


class ThreadBlockParser(HTMLParser):
    """Simpler approach: extract threads by finding thread-title anchors and
    nearby metadata using state machine parsing."""

    def __init__(self):
        super().__init__()
        self.threads = []
        self._state = "idle"
        self._current = {}
        self._buf = ""
        self._badge_kind = None

    def _flush(self):
        if self._current.get("subject"):
            self.threads.append(self._current)
        self._current = {"subject": "", "author": "", "date": "", "participants": 0, "replies": 0, "thread_url": ""}

    def handle_starttag(self, tag, attrs):
        ad = dict(attrs)
        cls = ad.get("class", "")

        # New thread block
        if tag == "div" and cls.strip() == "thread":
            self._flush()

        # Thread title link
        if tag == "a" and "thread-title" in cls:
            self._state = "title"
            self._buf = ""
            href = ad.get("href", "")
            self._current["thread_url"] = href

        # Sender name
        if tag == "span" and "sender-name" in cls:
            self._state = "sender"
            self._buf = ""

        # Thread date (date is in the title attribute)
        if tag == "span" and "thread-date" in cls:
            title = ad.get("title", "")
            if title:
                self._current["date"] = title

        # Badge icons identify what the next badge number means
        if tag == "i":
            aria = ad.get("aria-label", "")
            if aria == "participants":
                self._badge_kind = "participants"
            elif aria == "replies":
                self._badge_kind = "replies"

        # Badge span (contains the count number)
        if tag == "span" and "badge" in cls:
            self._state = "badge"
            self._buf = ""

    def handle_endtag(self, tag):
        if self._state == "title" and tag == "a":
            self._current["subject"] = self._buf.strip()
            self._state = "idle"
        elif self._state == "sender" and tag == "span":
            author = self._buf.strip()
            if author.startswith("by "):
                author = author[3:]
            self._current["author"] = author
            self._state = "idle"
        elif self._state == "badge" and tag == "span":
            num = re.search(r"\d+", self._buf)
            if num and self._badge_kind:
                self._current[self._badge_kind] = int(num.group())
            self._badge_kind = None
            self._state = "idle"

    def handle_data(self, data):
        if self._state in ("title", "sender", "badge"):
            self._buf += data

    def close(self):
        super().close()
        self._flush()
        # Remove the empty initial entry if present
        self.threads = [t for t in self.threads if t.get("subject")]


def parse_date(date_str):
    """Parse dates like 'Monday, 4 May 2026 08:11:19' to YYYY-MM-DD."""
    # Remove weekday prefix
    m = re.search(r"(\d{1,2})\s+(\w+)\s+(\d{4})", date_str)
    if m:
        day, month_name, year = m.groups()
        months = {
            "January": 1, "February": 2, "March": 3, "April": 4,
            "May": 5, "June": 6, "July": 7, "August": 8,
            "September": 9, "October": 10, "November": 11, "December": 12,
        }
        month_num = months.get(month_name, 0)
        if month_num:
            return f"{year}-{month_num:02d}-{int(day):02d}"
    return date_str


def fetch_ml_month(year, month):
    """Fetch and parse one month of mailing list threads."""
    url = f"{ML_BASE}/{year}/{month}/"
    req = urllib.request.Request(url, headers={"User-Agent": "ghc-proposals-dashboard"})
    try:
        with urllib.request.urlopen(req) as resp:
            html = resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError:
        return []

    parser = ThreadBlockParser()
    parser.feed(html)
    parser.close()
    return parser.threads


def fetch_mailing_list(months=6):
    """Fetch mailing list threads for the last N months."""
    all_threads = []
    now = datetime.now()
    seen = set()
    for i in range(months):
        dt = now - timedelta(days=i * 30)
        ym = (dt.year, dt.month)
        if ym in seen:
            continue
        seen.add(ym)
        print(f"  Scraping mailing list {ym[0]}/{ym[1]:02d}...", file=sys.stderr)
        threads = fetch_ml_month(ym[0], ym[1])
        all_threads.extend(threads)
        time.sleep(0.5)  # be polite
    return all_threads


def is_shepherd_thread(subject):
    """Check if a thread subject indicates a shepherd assignment. Returns PR number or None."""
    for pat in SHEPHERD_SUBJECT_PATTERNS:
        m = pat.search(subject)
        if m:
            return int(m.group(1))
    return None


def fetch_shepherd_from_thread(thread_url):
    """Fetch a thread page and extract the shepherd name from the email body."""
    url = f"https://mailman.haskell.org{thread_url}" if thread_url.startswith("/") else thread_url
    req = urllib.request.Request(url, headers={"User-Agent": "ghc-proposals-dashboard"})
    try:
        with urllib.request.urlopen(req) as resp:
            html = resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError:
        return None

    for pat in SHEPHERD_BODY_PATTERNS:
        m = pat.search(html)
        if m:
            name = m.group(1).strip().rstrip(" *")
            # Clean up any HTML remnants
            name = re.sub(r"<[^>]+>", "", name).strip()
            return name
    return None


def enrich_with_ml(proposals, ml_threads):
    """Enrich proposals with mailing list data."""
    # Build per-PR aggregation
    pr_threads = {}  # pr_number -> list of thread info
    shepherd_thread_urls = {}  # pr_number -> thread_url (for fetching shepherd name)

    for thread in ml_threads:
        subject = thread.get("subject", "")
        date_str = parse_date(thread.get("date", ""))
        participants = thread.get("participants", 0)

        # Check for shepherd assignment thread
        pr_num = is_shepherd_thread(subject)
        if pr_num and thread.get("thread_url"):
            shepherd_thread_urls[pr_num] = thread["thread_url"]

        # Find all PR references in subject
        for m in PR_REF_PATTERN.finditer(subject):
            pr_num = int(m.group(1))
            if pr_num not in pr_threads:
                pr_threads[pr_num] = []
            pr_threads[pr_num].append({
                "date": date_str,
                "participants": participants,
            })

    # Fetch shepherd names from assignment threads
    shepherds = {}
    if shepherd_thread_urls:
        print(f"  Fetching {len(shepherd_thread_urls)} shepherd assignment threads...", file=sys.stderr)
        for pr_num, thread_url in shepherd_thread_urls.items():
            name = fetch_shepherd_from_thread(thread_url)
            if name:
                shepherds[pr_num] = name
            time.sleep(0.3)

    # Apply to proposals
    for num, prop in proposals.items():
        if num in shepherds:
            prop["shepherd"] = shepherds[num]
        threads = pr_threads.get(num, [])
        prop["ml_threads"] = len(threads)
        if threads:
            dates = [t["date"] for t in threads if t["date"]]
            if dates:
                prop["ml_last_activity"] = max(dates)
            prop["ml_participants"] = max((t["participants"] for t in threads), default=0)


# ---------------------------------------------------------------------------
# Output formatters
# ---------------------------------------------------------------------------

COLUMNS = [
    ("PR #", "number", 6),
    ("Title", "title", 35),
    ("Status", "status", 18),
    ("Author", "author", 14),
    ("Shepherd", "shepherd", 14),
    ("Inbox Date", "created_at", 10),
    ("Last GH", "updated_at", 10),
    ("ML#", "ml_threads", 3),
    ("ML Last", "ml_last_activity", 10),
    ("Ppl", "ml_participants", 3),
]


def sort_proposals(proposals):
    """Sort proposals: pending items first (by date), then needs revision, then rest."""
    status_order = {
        "Pending Shepherd": 0,
        "Pending Committee": 1,
        "Needs Revision": 2,
        "Dormant": 3,
        "Open": 4,
        "Accepted": 5,
        "Merged": 6,
        "Rejected": 7,
    }
    return sorted(
        proposals.values(),
        key=lambda p: (status_order.get(p["status"], 99), p["created_at"]),
    )


def format_ascii(proposals):
    """Format proposals as an ASCII table."""
    rows = sort_proposals(proposals)
    if not rows:
        return "No proposals found.\n"

    # Build formatted rows
    formatted = []
    for row in rows:
        fmt = []
        for header, key, width in COLUMNS:
            val = row.get(key, "")
            if key == "number":
                val = f"#{val}"
            val = str(val)
            if len(val) > width:
                val = val[: width - 2] + ".."
            fmt.append(val)
        formatted.append(fmt)

    # Calculate actual widths
    widths = [max(len(COLUMNS[i][0]), max((len(r[i]) for r in formatted), default=0)) for i in range(len(COLUMNS))]

    def sep_line():
        return "+" + "+".join("-" * (w + 2) for w in widths) + "+"

    def data_line(cells):
        return "|" + "|".join(f" {cells[i]:<{widths[i]}} " for i in range(len(cells))) + "|"

    lines = [sep_line(), data_line([c[0] for c in COLUMNS]), sep_line()]
    for row in formatted:
        lines.append(data_line(row))
    lines.append(sep_line())
    return "\n".join(lines) + "\n"


def format_csv(proposals):
    """Format proposals as CSV."""
    rows = sort_proposals(proposals)
    buf = io.StringIO()
    writer = csv.writer(buf)
    writer.writerow([c[0] for c in COLUMNS] + ["URL"])
    for row in rows:
        writer.writerow(
            [f"#{row['number']}" if k == "number" else str(row.get(k, "")) for _, k, _ in COLUMNS]
            + [row.get("gh_url", "")]
        )
    return buf.getvalue()


def format_html(proposals):
    """Format proposals as a self-contained HTML page with sortable/filterable table."""
    rows = sort_proposals(proposals)

    def esc(s):
        return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")

    thead = "".join(f'<th onclick="sortTable({i})">{esc(c[0])}</th>' for i, c in enumerate(COLUMNS))

    tbody_rows = []
    for row in rows:
        cells = []
        for _, key, _ in COLUMNS:
            val = row.get(key, "")
            if key == "number":
                val = f'<a href="{esc(row.get("gh_url", ""))}" target="_blank">#{val}</a>'
                cells.append(f"<td>{val}</td>")
            else:
                cells.append(f"<td>{esc(val)}</td>")
        status_attr = esc(row.get("status", ""))
        tbody_rows.append(f'<tr data-status="{status_attr}">{"".join(cells)}</tr>')

    # Collect unique statuses for filter buttons
    statuses = sorted(set(r.get("status", "") for r in rows))
    filter_buttons = '<button class="filter-btn active" onclick="filterStatus(\'all\')">All</button>'
    for s in statuses:
        filter_buttons += f'<button class="filter-btn" onclick="filterStatus(\'{esc(s)}\')">{esc(s)}</button>'

    generated = datetime.now().strftime("%Y-%m-%d %H:%M")

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>GHC Proposals Dashboard</title>
<style>
  * {{ box-sizing: border-box; margin: 0; padding: 0; }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; padding: 1em; background: #f5f5f5; color: #333; }}
  h1 {{ margin-bottom: 0.3em; }}
  .meta {{ color: #666; font-size: 0.85em; margin-bottom: 1em; }}
  .filters {{ margin-bottom: 1em; }}
  .filter-btn {{ padding: 4px 12px; margin: 2px; border: 1px solid #ccc; background: #fff; border-radius: 4px; cursor: pointer; font-size: 0.85em; }}
  .filter-btn.active {{ background: #0366d6; color: #fff; border-color: #0366d6; }}
  table {{ border-collapse: collapse; width: 100%; background: #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
  th, td {{ border: 1px solid #ddd; padding: 6px 10px; text-align: left; font-size: 0.85em; white-space: nowrap; }}
  th {{ background: #f0f0f0; cursor: pointer; user-select: none; position: sticky; top: 0; }}
  th:hover {{ background: #e0e0e0; }}
  tr:nth-child(even) {{ background: #fafafa; }}
  tr:hover {{ background: #e8f0fe; }}
  a {{ color: #0366d6; text-decoration: none; }}
  a:hover {{ text-decoration: underline; }}
  td:nth-child(2) {{ max-width: 300px; overflow: hidden; text-overflow: ellipsis; }}
  .sort-arrow {{ font-size: 0.7em; margin-left: 4px; }}
</style>
</head>
<body>
<h1>GHC Proposals Dashboard</h1>
<p class="meta">Generated {esc(generated)} &mdash; {len(rows)} proposals</p>
<div class="filters">{filter_buttons}</div>
<table id="proposals">
<thead><tr>{thead}</tr></thead>
<tbody>{"".join(tbody_rows)}</tbody>
</table>
<script>
let sortCol = -1, sortAsc = true;
function sortTable(col) {{
  const tbody = document.querySelector("#proposals tbody");
  const rows = Array.from(tbody.querySelectorAll("tr"));
  if (sortCol === col) sortAsc = !sortAsc;
  else {{ sortCol = col; sortAsc = true; }}
  rows.sort((a, b) => {{
    let va = a.cells[col].textContent.trim(), vb = b.cells[col].textContent.trim();
    let na = parseFloat(va.replace('#','')), nb = parseFloat(vb.replace('#',''));
    if (!isNaN(na) && !isNaN(nb)) return sortAsc ? na - nb : nb - na;
    return sortAsc ? va.localeCompare(vb) : vb.localeCompare(va);
  }});
  rows.forEach(r => tbody.appendChild(r));
}}
function filterStatus(status) {{
  document.querySelectorAll(".filter-btn").forEach(b => b.classList.remove("active"));
  event.target.classList.add("active");
  document.querySelectorAll("#proposals tbody tr").forEach(r => {{
    r.style.display = (status === "all" || r.dataset.status === status) ? "" : "none";
  }});
}}
</script>
</body>
</html>"""


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="GHC Proposals Shepherd Dashboard")
    parser.add_argument("--closed-months", type=int, default=None, help="Include closed PRs from last N months")
    parser.add_argument("--format", choices=["ascii", "csv", "html"], default="ascii", help="Output format")
    parser.add_argument("--output", type=str, default=None, help="Output file")
    parser.add_argument("--github-token", type=str, default=None, help="GitHub token")
    parser.add_argument("--ml-months", type=int, default=6, help="Months of mailing list to scrape")
    parser.add_argument("--no-ml", action="store_true", help="Skip mailing list scraping")
    args = parser.parse_args()

    token = args.github_token or os.environ.get("GITHUB_TOKEN")
    proposals = fetch_proposals(token=token, closed_months=args.closed_months)
    print(f"Found {len(proposals)} proposals from GitHub.", file=sys.stderr)

    if not args.no_ml:
        print("Scraping mailing list...", file=sys.stderr)
        ml_threads = fetch_mailing_list(months=args.ml_months)
        print(f"Found {len(ml_threads)} mailing list threads.", file=sys.stderr)
        enrich_with_ml(proposals, ml_threads)

    if args.format == "ascii":
        output = format_ascii(proposals)
    elif args.format == "csv":
        output = format_csv(proposals)
    elif args.format == "html":
        output = format_html(proposals)

    if args.output:
        with open(args.output, "w") as f:
            f.write(output)
        print(f"Written to {args.output}", file=sys.stderr)
    else:
        if args.format == "html" and not args.output:
            default_out = "proposals.html"
            with open(default_out, "w") as f:
                f.write(output)
            print(f"Written to {default_out}", file=sys.stderr)
        else:
            sys.stdout.write(output)


if __name__ == "__main__":
    main()

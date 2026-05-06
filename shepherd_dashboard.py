#!/usr/bin/env python3
"""
GHC Proposals Shepherd Dashboard

Zero-dependency script that aggregates proposal status from:
  1. GitHub Search API (PR metadata, labels, dates)
  2. ghc-steering-committee mailing list archive (mbox bulk download)

Mbox files are downloaded once per month into ~/.cache/shepherd_dashboard/ ;
past months are cached forever, the current month is always re-fetched.

Usage:
  python3 shepherd_dashboard.py [OPTIONS]

Options:
  --closed-months N    Include closed/merged PRs from last N months (default: open only)
  --format FORMAT      Output: ascii (default), csv, html
  --output FILE        Output file (default: stdout for ascii/csv, proposals.html for html)
  --github-token TOKEN Override GITHUB_TOKEN env var
  --ml-months N        Months of mailing list to fetch (default: 18)
  --no-ml              Skip mailing list download (also disables CI validation)
"""

import argparse
import csv
import email
import email.policy
import email.utils
import gzip
import io
import itertools
import json
import mailbox
import os
import re
import shutil
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone
from pathlib import Path


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
    """Run a GitHub search query, paginating through all results.

    The search API limits unauthenticated callers to 10 req/min (6 s window)
    and authenticated callers to 30 req/min (2 s window). We sleep between
    pages to stay under the limit.
    """
    page_sleep = 2 if token else 7
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
        time.sleep(page_sleep)
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
        time.sleep(2 if token else 7)
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
        "votes": {},
        "votes_summary": "",
    }


# ---------------------------------------------------------------------------
# Mailing List (mbox bulk download from HyperKitty)
# ---------------------------------------------------------------------------

LIST_NAME = "ghc-steering-committee@haskell.org"
ML_EXPORT_BASE = f"https://mailman.haskell.org/archives/list/{LIST_NAME}/export"

# Patterns for shepherd assignment thread subjects
SHEPHERD_SUBJECT_PATTERNS = [
    re.compile(r"Assigning\s+Shepherd.*?#(\d+)", re.IGNORECASE),
    re.compile(r"Please\s+review\s+#(\d+)", re.IGNORECASE),
    re.compile(r"Shepherd.*?#(\d+)", re.IGNORECASE),
]

# Patterns for shepherd name in email body. Each must capture the name in group 1.
# We strip surrounding asterisks/whitespace afterwards, so patterns can be loose.
SHEPHERD_BODY_PATTERNS = [
    re.compile(r"nominate\s+([^.\n]{2,60}?)\s+as\s+(?:the\s+)?shepherd", re.IGNORECASE),
    re.compile(r"nominating\s+([^.\n]{2,60}?)\s+as\s+(?:the\s+)?shepherd", re.IGNORECASE),
    re.compile(r"([A-Z][^.\n,]{1,40}?)\s+(?:has\s+)?agreed\s+to\s+(?:be\s+(?:the\s+)?)?shepherd", re.IGNORECASE),
    re.compile(r"([A-Z][^.\n,]{1,40}?)\s+will\s+(?:be\s+the\s+)?shepherd", re.IGNORECASE),
    re.compile(r"shepherd\s+(?:will\s+be|is)\s+([^.\n,]{2,40})", re.IGNORECASE),
    re.compile(r"assigning\s+([^.\n,]{2,40}?)\s+as\s+(?:the\s+)?shepherd", re.IGNORECASE),
]

# Known shepherd names (current + recent past committee members).
# Format: (canonical_name, [aliases_for_matching]).
# Aliases include first names so "nominate Matthias" matches the full name.
COMMITTEE_MEMBERS = [
    ("Simon Marlow",                ["Simon Marlow", "Simon M"]),
    ("Simon Peyton Jones",          ["Simon Peyton Jones", "Simon Peyton-Jones", "Simon PJ", "SPJ"]),
    ("Adam Gundry",                 ["Adam Gundry", "Adam G", "Adam"]),
    ("Malte Ott",                   ["Malte Ott", "Malte"]),
    ("Matthías Páll Gissurarson",   ["Matthías Páll Gissurarson", "Matthias Pall Gissurarson", "Matthías", "Matthias"]),
    ("Erik de Castro Lopo",         ["Erik de Castro Lopo", "Erik D", "Erik"]),
    ("Sebastian Graf",              ["Sebastian Graf", "Sebastian"]),
    ("Jaro Reinders",               ["Jaro Reinders", "Jaro"]),
    ("Jeff Young",                  ["Jeff Young", "Jeff"]),
    ("Rodrigo Mesquita",            ["Rodrigo Mesquita", "Rodrigo"]),
    # Past members who may appear as shepherds in older threads
    ("Eric Seidel",                 ["Eric Seidel", "Eric S", "Eric"]),
    ("Jakob Brünker",               ["Jakob Brünker", "Jakob Bruenker", "Jakob"]),
    ("Moritz Angermann",            ["Moritz Angermann", "Moritz"]),
    ("Arnaud Spiwack",              ["Arnaud Spiwack", "Arnaud"]),
    ("Richard Eisenberg",           ["Richard Eisenberg", "Richard"]),
    ("Chris Dornan",                ["Chris Dornan", "Chris"]),
    ("Vladislav Zavialov",          ["Vladislav Zavialov", "Vlad Z", "Vlad"]),
]

# Pattern for PR number references in subjects
PR_REF_PATTERN = re.compile(r"#(\d+)")


def is_shepherd_thread(subject):
    """Check if a thread subject indicates a shepherd assignment. Returns PR number or None."""
    for pat in SHEPHERD_SUBJECT_PATTERNS:
        m = pat.search(subject)
        if m:
            return int(m.group(1))
    return None


def strip_html_to_text(html):
    """Strip HTML tags and decode common entities. Used for HTML-bodied messages."""
    text = re.sub(r"<[^>]+>", " ", html)
    text = text.replace("&nbsp;", " ").replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">").replace("&#x27;", "'").replace("&quot;", '"')
    return text


def normalize_candidate(name):
    """Clean noise (asterisks, brackets, whitespace) from a captured name."""
    name = name.strip()
    name = name.strip("*_~`'\"")
    name = re.sub(r"\s+", " ", name).strip()
    return name


def match_committee_member(candidate):
    """Match a captured candidate name against the committee list.

    Returns the canonical name if a match is found, else None.
    """
    if not candidate:
        return None
    cand_lower = candidate.lower()
    best = None
    best_len = 0
    for canonical, aliases in COMMITTEE_MEMBERS:
        for alias in aliases:
            if alias.lower() == cand_lower or cand_lower.startswith(alias.lower() + " "):
                if len(alias) > best_len:
                    best = canonical
                    best_len = len(alias)
    if best:
        return best
    first_token = candidate.split()[0].lower() if candidate.split() else ""
    for canonical, aliases in COMMITTEE_MEMBERS:
        for alias in aliases:
            if alias.lower() == first_token:
                return canonical
    return None


# ---------------------------------------------------------------------------
# Vote classification (minimal: inflection patterns + first-vote-per-member)
# ---------------------------------------------------------------------------
# FUTURE IMPROVEMENTS (deferred unless the calibration loop demonstrates need):
#   - Negation handling within sentence scope ("I don't accept" → reject)
#   - Sentence segmentation
#   - First-person anchoring for ambiguous verbs (`\bI\s+support\b`)
#   - Subject-line `recommendation: X` patterns for shepherd recs
#   - Naive Bayes trained on the calibration corpus

_VOTE_ACCEPT = re.compile(
    r"(?:"
    r"\baccept(?:ed|ing|s|able|ance)?\b|"
    r"\bapprov(?:e|ed|al|es)\b|"
    r"\bagree(?:d|s)?\b|"
    r"\bin\s+favou?r\b|"
    r"\bin\s+support\b|"
    r"\bno\s+objection(?:s)?\b|"
    r"\bhappy\s+(?:with|to\s+\w+|to\s+see)\b|"
    r"\bfine\s+with\b|"
    r"\bsupportive\b|"
    r"\bI(?:\s+(?:will|would|also|too))?\s+support\b|"
    r"\bsounds\s+(?:good|great|like\s+a\s+great)\b|"
    r"\b(?:LGTM|SGTM|WFM)\b|"
    r"(?<![\w-])\+1(?!\d)"
    r")",
    re.IGNORECASE,
)
_VOTE_REJECT = re.compile(
    r"(?:"
    r"\breject(?:ed|ing|s|ion)?\b|"
    r"\boppose(?:d|s)?\b|"
    r"\bvote\s+(?:against|no)\b|"
    r"\bnack\b|"
    r"(?<![\w-])-1(?!\d)"
    r")",
    re.IGNORECASE,
)
_VOTE_RECUSE = re.compile(
    r"\b(?:recus(?:e|ed|ing)|abstain(?:ed|ing|s)?)\b",
    re.IGNORECASE,
)
_VOTE_CONCERN = re.compile(
    r"\b(?:concern(?:s|ed)?|worried|hesitant|unconvinced|reservation(?:s)?)\b",
    re.IGNORECASE,
)


def classify_vote(body):
    """Classify a message body as accept | reject | recuse | concern | unclear.

    Returns (vote, confidence). Confidence is 1.0 for a single-category match
    or unanimous multi-match; 0.7 when accept and reject both match and one
    dominates by count; 0.0 for unclear.

    Decision rule:
      1. RECUSE keyword wins outright (strongest precision in the corpus).
      2. ACCEPT vs REJECT: count occurrences. The one with more wins; ties go
         to UNCLEAR (intentional — a tied message is a discussion, not a vote).
      3. CONCERN keyword present without accept/reject → CONCERN.
    """
    if not body:
        return "unclear", 0.0
    accept_n = len(_VOTE_ACCEPT.findall(body))
    reject_n = len(_VOTE_REJECT.findall(body))
    recuse_n = len(_VOTE_RECUSE.findall(body))
    concern_n = len(_VOTE_CONCERN.findall(body))

    if recuse_n > 0:
        confidence = 1.0 if (accept_n + reject_n + concern_n) == 0 else 0.7
        return "recuse", confidence

    if accept_n > 0 and reject_n > 0:
        if accept_n > reject_n:
            return "accept", 0.7
        if reject_n > accept_n:
            return "reject", 0.7
        return "unclear", 0.0  # tie

    if accept_n > 0:
        return "accept", 1.0 if concern_n == 0 else 0.7
    if reject_n > 0:
        return "reject", 1.0 if concern_n == 0 else 0.7

    if concern_n > 0:
        return "concern", 1.0

    return "unclear", 0.0


_PRONOUN_SHEPHERD = re.compile(r"^(?:myself|me|I)$", re.IGNORECASE)


def extract_shepherd_from_body(body, sender_name=None):
    """Extract a shepherd name from a message body.

    Returns (canonical_name, raw_match) where canonical_name is None if the
    captured text didn't validate against the committee member list.

    If `sender_name` is provided, pronouns like "myself"/"me"/"I" are
    resolved to the sender (e.g., "I'd like to nominate myself as shepherd").
    """
    if not body:
        return None, None
    # Strip emphasis markers so they don't fragment regex matches
    text = re.sub(r"[*_~`]", " ", body)
    text = re.sub(r"\s+", " ", text)

    def resolve(raw):
        # Pronoun → sender
        if sender_name and _PRONOUN_SHEPHERD.match(raw):
            return match_committee_member(sender_name)
        return match_committee_member(raw)

    for pat in SHEPHERD_BODY_PATTERNS:
        for m in pat.finditer(text):
            raw = normalize_candidate(m.group(1))
            canonical = resolve(raw)
            if canonical:
                return canonical, raw
    for pat in SHEPHERD_BODY_PATTERNS:
        m = pat.search(text)
        if m:
            return None, normalize_candidate(m.group(1))
    return None, None


# --- mbox download / cache --------------------------------------------------


def cache_dir():
    """Local cache for mbox files."""
    return Path(os.path.expanduser("~/.cache/shepherd_dashboard"))


def iter_months_back(n):
    """Yield (year, month) tuples for the last n months, current first."""
    now = datetime.now()
    y, m = now.year, now.month
    for _ in range(n):
        yield (y, m)
        m -= 1
        if m == 0:
            y -= 1
            m = 12


def _mbox_url(year, month):
    """Build the export URL for one calendar month."""
    end_y, end_m = (year + 1, 1) if month == 12 else (year, month + 1)
    return (
        f"{ML_EXPORT_BASE}/{LIST_NAME}-{year}-{month:02d}.mbox.gz"
        f"?start={year:04d}-{month:02d}-01"
        f"&end={end_y:04d}-{end_m:02d}-01"
    )


def download_one_month(year, month, force=False):
    """Download mbox for (year, month) into cache. Returns local mbox path or None.

    Past months are cached forever (immutable archive). The current month is
    re-downloaded each call to pick up recent traffic.
    """
    cache = cache_dir()
    cache.mkdir(parents=True, exist_ok=True)
    gz_path = cache / f"{LIST_NAME}-{year:04d}-{month:02d}.mbox.gz"
    mbox_path = cache / f"{LIST_NAME}-{year:04d}-{month:02d}.mbox"

    now = datetime.now()
    is_current = (year, month) == (now.year, now.month)
    needs_download = force or is_current or not gz_path.exists()

    if needs_download:
        url = _mbox_url(year, month)
        req = urllib.request.Request(url, headers={"User-Agent": "ghc-proposals-dashboard"})
        try:
            with urllib.request.urlopen(req) as resp:
                gz_path.write_bytes(resp.read())
        except urllib.error.HTTPError as e:
            if e.code == 404:
                # No traffic for this month
                return None
            raise

    if needs_download or not mbox_path.exists():
        # mailbox.mbox needs an uncompressed file. Decompress alongside.
        with gzip.open(gz_path, "rb") as f_in, open(mbox_path, "wb") as f_out:
            shutil.copyfileobj(f_in, f_out)

    return mbox_path


def download_archive(months):
    """Download up to `months` months back. Returns list of local mbox paths."""
    paths = []
    for y, m in iter_months_back(months):
        try:
            p = download_one_month(y, m)
        except Exception as e:
            print(f"  Warning: failed to download {y}/{m:02d}: {e}", file=sys.stderr)
            continue
        if p:
            paths.append(p)
        time.sleep(0.2)
    return paths


# --- mbox parsing -----------------------------------------------------------


def _clean_subject(subject):
    """Strip mailing-list prefix and Re: chains from a subject."""
    s = (subject or "").replace("\n", " ").replace("\r", " ").strip()
    s = re.sub(r"\[ghc-steering-committee\]\s*", "", s, flags=re.IGNORECASE)
    s = re.sub(r"^(?:Re:\s*|Fwd:\s*)+", "", s, flags=re.IGNORECASE)
    return re.sub(r"\s+", " ", s).strip()


_ATTRIBUTION_RE = re.compile(
    r"^(?:On\s+.+?(?:wrote|escreveu|napsal):|[A-Z][\w\s.,'-]{1,60}\s+(?:wrote|escreveu|napsal):)\s*$"
)
_LIST_FOOTER_RE = re.compile(r"_{5,}\s*\n.*?mailing\s+list", re.S | re.IGNORECASE)
_SIG_DASH_RE = re.compile(r"^--\s*$", re.MULTILINE)


def strip_quoted_text(body):
    """Strip quoted reply text, attribution lines, footer, and signature.

    Heuristics (tuned for this list):
      - lines starting with `>` are quoted; cut at first such line.
      - "On <date>, <name> wrote:" attribution → cut.
      - "<Name> wrote:" attribution → cut.
      - mailing list footer (`_______ ... mailing list`) → cut.
      - signature dash line (`-- `) → cut.
    """
    if not body:
        return ""
    lines = body.splitlines()
    cut = len(lines)
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        if stripped.startswith(">"):
            cut = i
            break
        if _ATTRIBUTION_RE.match(stripped):
            cut = i
            break
    text = "\n".join(lines[:cut])
    text = _LIST_FOOTER_RE.split(text)[0]
    sig = _SIG_DASH_RE.search(text)
    if sig:
        text = text[: sig.start()]
    return text.strip()


def _extract_plain_body(eml):
    """Extract the text/plain body from an email.EmailMessage. Falls back to HTML→text."""
    try:
        body_part = eml.get_body(preferencelist=("plain",))
        if body_part is not None:
            return body_part.get_content()
    except Exception:
        pass
    # Walk parts manually
    if eml.is_multipart():
        for part in eml.walk():
            if part.get_content_type() == "text/plain":
                try:
                    return part.get_content()
                except Exception:
                    payload = part.get_payload(decode=True)
                    if payload:
                        return payload.decode(errors="replace")
        # Last-ditch: HTML body
        for part in eml.walk():
            if part.get_content_type() == "text/html":
                try:
                    return strip_html_to_text(part.get_content())
                except Exception:
                    payload = part.get_payload(decode=True)
                    if payload:
                        return strip_html_to_text(payload.decode(errors="replace"))
    payload = eml.get_payload(decode=True)
    if isinstance(payload, bytes):
        return payload.decode(errors="replace")
    return str(payload or "")


def parse_message(eml):
    """Convert an email.EmailMessage to a Message dict, or None to skip."""
    name, addr = email.utils.parseaddr(eml.get("From", "") or "")
    subject = _clean_subject(eml.get("Subject", ""))
    date_raw = eml.get("Date", "")
    try:
        dt = email.utils.parsedate_to_datetime(date_raw)
    except (TypeError, ValueError):
        return None
    if dt is None:
        return None
    body = _extract_plain_body(eml)
    pr_refs = sorted({int(m.group(1)) for m in PR_REF_PATTERN.finditer(subject)})
    references = (eml.get("References") or "").split()
    return {
        "message_id": (eml.get("Message-ID") or "").strip(),
        "in_reply_to": (eml.get("In-Reply-To") or "").strip(),
        "references": references,
        "from_name": name,
        "from_email": addr.lower(),
        "subject": subject,
        "date": dt.strftime("%Y-%m-%d"),
        "datetime": dt,
        "body": body,
        "clean_body": strip_quoted_text(body),
        "pr_refs": pr_refs,
    }


def parse_archive(paths):
    """Parse a list of mbox file paths into a flat list of Message dicts (sorted by date)."""
    messages = []
    for path in paths:
        try:
            mbox = mailbox.mbox(str(path))
        except Exception as e:
            print(f"  Warning: failed to open {path}: {e}", file=sys.stderr)
            continue
        for raw in mbox:
            try:
                eml = email.message_from_bytes(bytes(raw), policy=email.policy.default)
                m = parse_message(eml)
                if m:
                    messages.append(m)
            except Exception as e:
                print(f"  Warning: failed to parse a message in {path.name}: {e}", file=sys.stderr)
    # Aware/naive datetime mix can occur; normalise for sorting
    def _sort_key(m):
        dt = m["datetime"]
        if dt.tzinfo is None:
            return dt.replace(tzinfo=timezone.utc)
        return dt
    messages.sort(key=_sort_key)
    return messages


def enrich_with_ml(proposals, messages):
    """Enrich proposals with mailing list data from parsed mbox messages.

    Returns a dict of pr_num -> raw_match for shepherd names parsed from the
    mailing list that did not validate against COMMITTEE_MEMBERS. Callers
    should treat a non-empty result as an error condition.
    """
    by_msgid = {m["message_id"]: m for m in messages if m["message_id"]}

    # PR-tagging: a message is "about" PR #N if its (cleaned) subject mentions #N,
    # OR its parent (via In-Reply-To/References) is about #N. This catches replies
    # whose subject was edited to drop the PR reference.
    pr_messages = {}  # pr_number -> list of messages
    shepherd_assignments = {}  # pr_number -> (datetime, body) of latest assignment thread root

    def pr_refs_for(msg):
        if msg["pr_refs"]:
            return msg["pr_refs"]
        parent = by_msgid.get(msg["in_reply_to"])
        if parent:
            return pr_refs_for(parent)
        for ref in msg["references"]:
            parent = by_msgid.get(ref)
            if parent and parent["pr_refs"]:
                return parent["pr_refs"]
        return []

    for msg in messages:
        for pr_num in pr_refs_for(msg):
            pr_messages.setdefault(pr_num, []).append(msg)

        # Shepherd assignment: subject must match AND this is a thread root
        # (no In-Reply-To). Replies don't carry the assignment body.
        if not msg["in_reply_to"]:
            pr = is_shepherd_thread(msg["subject"])
            if pr is not None:
                existing = shepherd_assignments.get(pr)
                if not existing or msg["datetime"] > existing[0]:
                    shepherd_assignments[pr] = (
                        msg["datetime"],
                        msg["clean_body"] or msg["body"],
                        msg["from_name"],
                    )

    # Resolve shepherd names
    shepherds = {}
    unverified = {}
    for pr_num, (_dt, body, from_name) in shepherd_assignments.items():
        canonical, raw = extract_shepherd_from_body(body, sender_name=from_name)
        if canonical:
            shepherds[pr_num] = canonical
        elif raw:
            unverified[pr_num] = raw

    # Per-PR vote aggregation: first non-unclear classification per member wins.
    pr_votes = {}  # pr_num -> {canonical_member: vote}
    for pr_num, msgs in pr_messages.items():
        votes_for_pr = {}
        # Walk messages in chronological order (already sorted by parse_archive)
        for msg in msgs:
            member = match_committee_member(msg["from_name"])
            if not member:
                continue
            if member in votes_for_pr:
                continue  # first vote wins
            body = msg["clean_body"] or msg["body"]
            vote, conf = classify_vote(body)
            if vote != "unclear":
                votes_for_pr[member] = vote
        if votes_for_pr:
            pr_votes[pr_num] = votes_for_pr

    for num, prop in proposals.items():
        if num in shepherds:
            prop["shepherd"] = shepherds[num]
        elif num in unverified:
            prop["shepherd"] = f"?{unverified[num]}"
        msgs = pr_messages.get(num, [])
        prop["ml_threads"] = len(msgs)
        if msgs:
            prop["ml_last_activity"] = max(m["date"] for m in msgs)
            unique_senders = {m["from_email"] for m in msgs if m["from_email"]}
            prop["ml_participants"] = len(unique_senders)
        prop["votes"] = pr_votes.get(num, {})
        prop["votes_summary"] = format_vote_summary(prop["votes"])

    return unverified


def format_vote_summary(votes):
    """Compact summary like '5A 1R 1c 2x' for ASCII / CSV output.

    Codes:  A = accept, R = reject, c = concern, x = recuse.
    Empty string when no votes.
    """
    if not votes:
        return ""
    counts = {"accept": 0, "reject": 0, "concern": 0, "recuse": 0}
    for v in votes.values():
        if v in counts:
            counts[v] += 1
    parts = []
    if counts["accept"]:
        parts.append(f"{counts['accept']}A")
    if counts["reject"]:
        parts.append(f"{counts['reject']}R")
    if counts["concern"]:
        parts.append(f"{counts['concern']}c")
    if counts["recuse"]:
        parts.append(f"{counts['recuse']}x")
    return " ".join(parts)


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
    ("Votes", "votes_summary", 12),
]


def display_title(prop):
    """Return the title with an `[A] ` prefix if this proposal is an amendment."""
    title = prop.get("title", "")
    return f"[A] {title}" if prop.get("is_amendment") else title


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
            elif key == "title":
                val = display_title(row)
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
    """Format proposals as CSV (includes a Votes_Detail column for per-member breakdown)."""
    rows = sort_proposals(proposals)
    buf = io.StringIO()
    writer = csv.writer(buf)
    writer.writerow([c[0] for c in COLUMNS] + ["Votes_Detail", "URL"])
    for row in rows:
        cells = []
        for _, key, _ in COLUMNS:
            if key == "number":
                cells.append(f"#{row['number']}")
            elif key == "title":
                cells.append(display_title(row))
            else:
                cells.append(str(row.get(key, "")))
        votes = row.get("votes", {}) or {}
        cells.append(", ".join(f"{m}={v}" for m, v in sorted(votes.items())))
        cells.append(row.get("gh_url", ""))
        writer.writerow(cells)
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
            if key == "number":
                cells.append(f'<td><a href="{esc(row.get("gh_url", ""))}" target="_blank">#{row["number"]}</a></td>')
            elif key == "title":
                cells.append(f"<td>{esc(display_title(row))}</td>")
            elif key == "votes_summary":
                votes = row.get("votes", {}) or {}
                if votes:
                    detail = "\n".join(f"{m}: {v}" for m, v in sorted(votes.items()))
                    cells.append(f'<td title="{esc(detail)}">{esc(row.get(key, ""))}</td>')
                else:
                    cells.append(f"<td>{esc(row.get(key, ''))}</td>")
            else:
                cells.append(f"<td>{esc(row.get(key, ''))}</td>")
        status_attr = esc(row.get("status", ""))
        tbody_rows.append(f'<tr data-status="{status_attr}">{"".join(cells)}</tr>')

    # Collect unique statuses for filter buttons
    statuses = sorted(set(r.get("status", "") for r in rows))
    filter_buttons = '<button class="filter-btn active" onclick="filterStatus(event, \'all\')">All</button>'
    for s in statuses:
        filter_buttons += f'<button class="filter-btn" onclick="filterStatus(event, \'{esc(s)}\')">{esc(s)}</button>'

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
  details.legend {{ background: #fff; border: 1px solid #ddd; border-radius: 4px; padding: 0.5em 0.8em; margin-bottom: 1em; font-size: 0.85em; }}
  details.legend > summary {{ cursor: pointer; font-weight: 600; color: #0366d6; user-select: none; }}
  details.legend > summary:hover {{ color: #024185; }}
  details.legend dl {{ margin-top: 0.6em; display: grid; grid-template-columns: max-content 1fr; gap: 0.2em 1em; }}
  details.legend dt {{ font-weight: 600; color: #333; }}
  details.legend dd {{ color: #555; }}
  details.legend p.note {{ margin-top: 0.6em; color: #666; font-style: italic; }}
</style>
</head>
<body>
<h1>GHC Proposals Dashboard</h1>
<p class="meta">Generated {esc(generated)} &mdash; {len(rows)} proposals</p>
<details class="legend">
  <summary>Column reference</summary>
  <dl>
    <dt>PR #</dt><dd>GitHub PR number; click to open the PR.</dd>
    <dt>Title</dt><dd>Short title from the PR. <code>[A]</code> prefix marks an Amendment to a previously-accepted proposal.</dd>
    <dt>Status</dt><dd>Derived from GitHub labels: <em>Pending Shepherd</em>, <em>Pending Committee</em>, <em>Needs Revision</em>, <em>Accepted</em>, <em>Rejected</em>, <em>Dormant</em>, otherwise <em>Open</em> or <em>Merged</em>. Click a button above to filter.</dd>
    <dt>Author</dt><dd>GitHub username of the proposal author.</dd>
    <dt>Shepherd</dt><dd>Committee member assigned via the mailing list (parsed from "Assigning Shepherd" or "Please review" threads). A <code>?</code> prefix means a name was parsed but did not match the known committee list.</dd>
    <dt>Inbox Date</dt><dd>When the PR was opened (proxy for "entered the committee inbox").</dd>
    <dt>Last GH</dt><dd>Most recent activity on the GitHub PR.</dd>
    <dt>ML#</dt><dd>Number of mailing-list messages in threads referencing this PR.</dd>
    <dt>ML Last</dt><dd>Most recent date of mailing-list activity for this PR.</dd>
    <dt>Ppl</dt><dd>Unique senders across the mailing-list messages for this PR.</dd>
    <dt>Votes</dt><dd>Compact tally per category: <code>NA</code> = N accepts, <code>NR</code> = N rejects, <code>Nc</code> = N concerns, <code>Nx</code> = N recuses. Hover the cell for the per-member breakdown. Heuristic regex classifier &mdash; treat low-confidence cells as a hint, not ground truth.</dd>
  </dl>
  <p class="note">Click a column header to sort. Vote classification accuracy on historical labels: accept P=97% R=88%, others are noisier due to small sample sizes.</p>
</details>
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
function filterStatus(e, status) {{
  document.querySelectorAll(".filter-btn").forEach(b => b.classList.remove("active"));
  e.target.classList.add("active");
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
    parser.add_argument("--ml-months", type=int, default=18, help="Months of mailing list to scrape (default: 18)")
    parser.add_argument("--no-ml", action="store_true", help="Skip mailing list scraping")
    args = parser.parse_args()

    token = args.github_token or os.environ.get("GITHUB_TOKEN")
    proposals = fetch_proposals(token=token, closed_months=args.closed_months)
    print(f"Found {len(proposals)} proposals from GitHub.", file=sys.stderr)

    unverified = {}
    missing_pending = []
    if args.no_ml:
        print(
            "Note: --no-ml is set; CI validation (shepherd names, missing pending shepherds) is not active.",
            file=sys.stderr,
        )
    if not args.no_ml:
        print("Downloading mailing list archive...", file=sys.stderr)
        months = args.ml_months
        paths = download_archive(months)
        print(f"Have {len(paths)} cached mbox month(s). Parsing...", file=sys.stderr)
        messages = parse_archive(paths)
        print(f"Parsed {len(messages)} messages.", file=sys.stderr)
        unverified = enrich_with_ml(proposals, messages)

        # Auto-extend window if pending proposals still have no shepherd.
        PENDING = {"Pending Shepherd", "Pending Committee"}
        max_months = 60
        while months < max_months:
            missing_now = [n for n, p in proposals.items() if p["status"] in PENDING and not p["shepherd"].lstrip("?")]
            if not missing_now:
                break
            extra = min(12, max_months - months)
            print(f"\n  Extending mbox archive by {extra} more months to find missing shepherds...", file=sys.stderr)
            new_paths = []
            for y, m in itertools.islice(iter_months_back(months + extra), months, None):
                try:
                    p = download_one_month(y, m)
                except Exception as e:
                    print(f"  Warning: failed to download {y}/{m:02d}: {e}", file=sys.stderr)
                    continue
                if p:
                    new_paths.append(p)
                time.sleep(0.2)
            paths.extend(new_paths)
            months += extra
            messages = parse_archive(paths)
            unverified = enrich_with_ml(proposals, messages)

        # Recompute final missing-pending after all extensions.
        missing_pending = [
            (n, p["status"]) for n, p in proposals.items()
            if p["status"] in PENDING and not p["shepherd"]
        ]

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

    # CI checks: report unverified shepherd names and pending-without-shepherd
    # as errors so this script can be run in a pipeline to catch:
    #   1. New committee members not yet added to COMMITTEE_MEMBERS
    #   2. Proposals stuck in pending states without a shepherd assignment
    exit_code = 0
    if unverified:
        print("\nERROR: shepherd-name validation failed for these proposals:", file=sys.stderr)
        for pr_num, raw in sorted(unverified.items()):
            print(f"    #{pr_num}: parsed name '{raw}' is not in COMMITTEE_MEMBERS", file=sys.stderr)
        print("    Add the parsed name to COMMITTEE_MEMBERS, or fix the body parser.", file=sys.stderr)
        exit_code = 1
    if missing_pending:
        print(f"\nERROR: {len(missing_pending)} proposal(s) in pending states have no shepherd:", file=sys.stderr)
        for pr_num, status in sorted(missing_pending):
            print(f"    #{pr_num} ({status})", file=sys.stderr)
        print("    A shepherd is required for Pending Shepherd / Pending Committee statuses.", file=sys.stderr)
        exit_code = 1
    sys.exit(exit_code)


if __name__ == "__main__":
    main()

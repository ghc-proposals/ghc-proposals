"""Unit tests for shepherd_dashboard.py pure functions."""

import unittest
from datetime import datetime, timezone

import shepherd_dashboard as sd


def _msg(from_name, body, day=1, **kw):
    """Minimal Message dict for directive-scanning tests."""
    m = {
        "message_id": kw.get("message_id", f"<{from_name}-{day}>"),
        "in_reply_to": "",
        "references": [],
        "from_name": from_name,
        "from_email": kw.get("from_email", ""),
        "subject": "",
        "date": f"2026-01-{day:02d}",
        "datetime": datetime(2026, 1, day, tzinfo=timezone.utc),
        "body": body,
        "clean_body": body,
        "pr_refs": [],
    }
    m.update({k: v for k, v in kw.items() if k in m})
    return m


class TestMatchCommitteeMember(unittest.TestCase):

    def test_exact_alias(self):
        self.assertEqual(sd.match_committee_member("Simon Marlow"), "Simon Marlow")
        self.assertEqual(sd.match_committee_member("Sebastian Graf"), "Sebastian Graf")

    def test_first_token(self):
        self.assertEqual(sd.match_committee_member("Sebastian"), "Sebastian Graf")
        self.assertEqual(sd.match_committee_member("Matthias"), "Matthías Páll Gissurarson")

    def test_short_form(self):
        self.assertEqual(sd.match_committee_member("Simon PJ"), "Simon Peyton Jones")
        self.assertEqual(sd.match_committee_member("Simon M"), "Simon Marlow")

    def test_case_insensitive(self):
        self.assertEqual(sd.match_committee_member("simon marlow"), "Simon Marlow")
        self.assertEqual(sd.match_committee_member("SEBASTIAN"), "Sebastian Graf")

    def test_unknown(self):
        self.assertIsNone(sd.match_committee_member("Random Stranger"))
        self.assertIsNone(sd.match_committee_member("myself"))

    def test_empty(self):
        self.assertIsNone(sd.match_committee_member(""))
        self.assertIsNone(sd.match_committee_member(None))

    def test_trailing_words(self):
        # "Sebastian Graf and friends" should still match Sebastian Graf
        self.assertEqual(
            sd.match_committee_member("Sebastian Graf and friends"),
            "Sebastian Graf",
        )

    def test_longest_alias_wins(self):
        # "Simon Peyton Jones" is more specific than "Simon"; picking the
        # right Simon matters.
        self.assertEqual(
            sd.match_committee_member("Simon Peyton Jones"),
            "Simon Peyton Jones",
        )


class TestFuzzyAndIdentifierMatch(unittest.TestCase):

    def test_identifier_handle(self):
        self.assertEqual(sd.match_committee_member("sgraf812"), "Sebastian Graf")
        self.assertEqual(sd.match_committee_member("@sgraf812"), "Sebastian Graf")
        self.assertEqual(sd.match_committee_member("Tritlo"), "Matthías Páll Gissurarson")

    def test_identifier_with_trailing_paren_token(self):
        self.assertEqual(sd.match_committee_member("sgraf812 (self)"), "Sebastian Graf")

    def test_fuzzy_typo_resolves(self):
        # opt-in only
        self.assertIsNone(sd.match_committee_member("Sebstian"))
        self.assertEqual(sd.match_committee_member("Sebstian", fuzzy=True), "Sebastian Graf")
        self.assertEqual(sd.match_committee_member("Sebastian Graff", fuzzy=True), "Sebastian Graf")

    def test_fuzzy_ambiguous_first_name_is_none(self):
        # Two Simons share a first name → never resolves, even fuzzily.
        self.assertIsNone(sd.match_committee_member("Simon", fuzzy=True))
        self.assertIsNone(sd.match_committee_member("Siomn", fuzzy=True))

    def test_fuzzy_unknown_is_none(self):
        self.assertIsNone(sd.match_committee_member("Complete Outsider", fuzzy=True))

    def test_fuzzy_does_not_overmatch_email(self):
        self.assertIsNone(sd.match_committee_member("nobody@example.com", fuzzy=True))


class TestParseStructuredVote(unittest.TestCase):

    def test_synonyms(self):
        self.assertEqual(sd.parse_structured_vote("accept"), "accept")
        self.assertEqual(sd.parse_structured_vote("Approved"), "accept")
        self.assertEqual(sd.parse_structured_vote("+1"), "accept")
        self.assertEqual(sd.parse_structured_vote("reject"), "reject")
        self.assertEqual(sd.parse_structured_vote("-1"), "reject")
        self.assertEqual(sd.parse_structured_vote("abstain"), "recuse")
        self.assertEqual(sd.parse_structured_vote("concern"), "concern")

    def test_leading_token_only(self):
        self.assertEqual(sd.parse_structured_vote("accept (reluctantly)"), "accept")
        self.assertEqual(sd.parse_structured_vote("reject, see thread"), "reject")

    def test_unknown(self):
        self.assertIsNone(sd.parse_structured_vote("maybe"))
        self.assertIsNone(sd.parse_structured_vote(""))


class TestScanStructuredDirectives(unittest.TestCase):

    def test_vote_attributed_to_sender(self):
        msgs = [_msg("Sebastian Graf", "Vote #1234: accept")]
        _shep, votes, _unv = sd.scan_structured_directives(msgs)
        self.assertEqual(votes, {1234: {"Sebastian Graf": "accept"}})

    def test_vote_last_wins(self):
        msgs = [
            _msg("Adam Gundry", "Vote #50: reject", day=1),
            _msg("Adam Gundry", "Vote #50: accept", day=2),
        ]
        _shep, votes, _unv = sd.scan_structured_directives(msgs)
        self.assertEqual(votes[50]["Adam Gundry"], "accept")

    def test_vote_ignores_non_member_sender(self):
        msgs = [_msg("Random Author", "Vote #1234: accept")]
        _shep, votes, _unv = sd.scan_structured_directives(msgs)
        self.assertEqual(votes, {})

    def test_vote_ignored_in_quoted_text(self):
        # scan reads clean_body, which has already had quoted lines stripped, so
        # a vote that only appears quoted in a reply never reaches us.
        msgs = [_msg("Adam Gundry", "I'm still thinking.")]
        _shep, votes, _unv = sd.scan_structured_directives(msgs)
        self.assertEqual(votes, {})

    def test_shepherd_by_full_name(self):
        msgs = [_msg("Adam Gundry", "Shepherd #77: Sebastian Graf")]
        shep, _votes, unv = sd.scan_structured_directives(msgs)
        self.assertEqual(shep, {77: "Sebastian Graf"})
        self.assertEqual(unv, {})

    def test_shepherd_by_handle(self):
        shep, _v, _u = sd.scan_structured_directives([_msg("Adam Gundry", "Shepherd #1: sgraf812")])
        self.assertEqual(shep[1], "Sebastian Graf")
        shep, _v, _u = sd.scan_structured_directives([_msg("Adam Gundry", "Shepherd #2: @maralorn")])
        self.assertEqual(shep[2], "Malte Ott")

    def test_shepherd_typo_resolves(self):
        shep, _v, unv = sd.scan_structured_directives([_msg("Adam Gundry", "Shepherd #3: Sebstian Graf")])
        self.assertEqual(shep[3], "Sebastian Graf")
        self.assertEqual(unv, {})

    def test_shepherd_unresolved_is_flagged(self):
        shep, _v, unv = sd.scan_structured_directives([_msg("Adam Gundry", "Shepherd #4: Complete Outsider")])
        self.assertNotIn(4, shep)
        self.assertEqual(unv[4], "Complete Outsider")

    def test_shepherd_last_wins(self):
        msgs = [
            _msg("Adam Gundry", "Shepherd #5: Adam Gundry", day=1),
            _msg("Simon Marlow", "Shepherd #5: Simon Marlow", day=2),
        ]
        shep, _v, _u = sd.scan_structured_directives(msgs)
        self.assertEqual(shep[5], "Simon Marlow")


class TestLevenshtein(unittest.TestCase):

    def test_basic(self):
        self.assertEqual(sd._levenshtein("kitten", "sitting"), 3)
        self.assertEqual(sd._levenshtein("", "abc"), 3)
        self.assertEqual(sd._levenshtein("abc", "abc"), 0)


class TestNormalizeCandidate(unittest.TestCase):

    def test_strips_asterisks(self):
        self.assertEqual(sd.normalize_candidate("*Sebastian Graf*"), "Sebastian Graf")
        self.assertEqual(sd.normalize_candidate("*Sebastian Graf "), "Sebastian Graf")

    def test_strips_underscores_backticks(self):
        self.assertEqual(sd.normalize_candidate("_Adam Gundry_"), "Adam Gundry")
        self.assertEqual(sd.normalize_candidate("`Erik`"), "Erik")

    def test_collapses_whitespace(self):
        self.assertEqual(sd.normalize_candidate("Simon   Marlow"), "Simon Marlow")
        self.assertEqual(sd.normalize_candidate("\tSimon\nMarlow "), "Simon Marlow")


class TestIsShepherdThread(unittest.TestCase):

    def test_assigning_shepherd(self):
        self.assertEqual(
            sd.is_shepherd_thread("Assigning Shepherd: #748 Unordered redundant commas"),
            748,
        )

    def test_please_review(self):
        self.assertEqual(
            sd.is_shepherd_thread("Please review #681: Updates to modifier proposal"),
            681,
        )

    def test_case_insensitive(self):
        self.assertEqual(
            sd.is_shepherd_thread("ASSIGNING SHEPHERD: #100"),
            100,
        )

    def test_no_match(self):
        self.assertIsNone(sd.is_shepherd_thread("Random discussion thread"))
        self.assertIsNone(sd.is_shepherd_thread("Re: GHC release schedule"))

    def test_no_pr_number(self):
        self.assertIsNone(sd.is_shepherd_thread("Assigning Shepherd: TBD"))


class TestStripHtmlToText(unittest.TestCase):

    def test_removes_tags(self):
        self.assertIn("hello", sd.strip_html_to_text("<p>hello</p>"))
        self.assertNotIn("<p>", sd.strip_html_to_text("<p>hello</p>"))

    def test_decodes_entities(self):
        self.assertIn("'", sd.strip_html_to_text("&#x27;"))
        self.assertIn('"', sd.strip_html_to_text("&quot;"))
        self.assertIn("&", sd.strip_html_to_text("&amp;"))


class TestStripQuotedText(unittest.TestCase):

    def test_removes_quoted_lines(self):
        body = "I think this is fine.\n\n> previous message line\n> another quoted line"
        cleaned = sd.strip_quoted_text(body)
        self.assertIn("I think this is fine", cleaned)
        self.assertNotIn("previous message", cleaned)

    def test_removes_attribution_line(self):
        body = (
            "Looks good to me.\n\n"
            "On Mon, 1 Jan 2026 at 10:00, Simon Marlow <s@example.com> wrote:\n"
            "> the original message"
        )
        cleaned = sd.strip_quoted_text(body)
        self.assertIn("Looks good to me", cleaned)
        self.assertNotIn("Simon Marlow", cleaned)
        self.assertNotIn("original message", cleaned)

    def test_strips_signature(self):
        body = "I accept this proposal.\n\n-- \nAdam Gundry, Haskell Consultant\nWell-Typed LLP"
        cleaned = sd.strip_quoted_text(body)
        self.assertIn("I accept", cleaned)
        self.assertNotIn("Well-Typed", cleaned)

    def test_strips_mailman_footer(self):
        body = (
            "fine with me\n\n"
            "_______________________________________________\n"
            "ghc-steering-committee mailing list -- ghc-steering-committee@haskell.org"
        )
        cleaned = sd.strip_quoted_text(body)
        self.assertIn("fine with me", cleaned)
        self.assertNotIn("mailing list", cleaned)


class TestExtractShepherdFromBody(unittest.TestCase):

    def test_basic_nominate(self):
        body = "I'd like to nominate Sebastian Graf as the shepherd."
        canonical, raw = sd.extract_shepherd_from_body(body)
        self.assertEqual(canonical, "Sebastian Graf")

    def test_nominate_with_asterisks(self):
        # The original failing case
        body = "I'd like to nominate *Sebastian Graf *as the shepherd."
        canonical, raw = sd.extract_shepherd_from_body(body)
        self.assertEqual(canonical, "Sebastian Graf")

    def test_nominate_first_name_only(self):
        body = "I'd like to nominate Matthias as the shepherd."
        canonical, raw = sd.extract_shepherd_from_body(body)
        self.assertEqual(canonical, "Matthías Páll Gissurarson")

    def test_pronoun_myself(self):
        body = "I'd like to nominate *myself *as the shepherd."
        canonical, raw = sd.extract_shepherd_from_body(body, sender_name="Simon Marlow")
        self.assertEqual(canonical, "Simon Marlow")

    def test_unknown_name(self):
        body = "I'd like to nominate Some Outsider as the shepherd."
        canonical, raw = sd.extract_shepherd_from_body(body)
        self.assertIsNone(canonical)
        self.assertEqual(raw, "Some Outsider")

    def test_empty(self):
        self.assertEqual(sd.extract_shepherd_from_body(""), (None, None))
        self.assertEqual(sd.extract_shepherd_from_body(None), (None, None))


class TestIterMonthsBack(unittest.TestCase):

    def test_returns_n_months(self):
        months = list(sd.iter_months_back(5))
        self.assertEqual(len(months), 5)

    def test_no_duplicates(self):
        months = list(sd.iter_months_back(24))
        self.assertEqual(len(months), len(set(months)))

    def test_year_rollover(self):
        # Generator must walk years backwards correctly
        months = list(sd.iter_months_back(15))
        ys = {y for y, _ in months}
        # 15 months span at least two distinct years
        self.assertGreaterEqual(len(ys), 2)

    def test_chronological_order(self):
        months = list(sd.iter_months_back(6))
        # Each step should be strictly earlier than the previous
        for prev, cur in zip(months, months[1:]):
            self.assertGreater(prev[0] * 12 + prev[1], cur[0] * 12 + cur[1])


class TestClassifyVote(unittest.TestCase):

    def test_accept_basic(self):
        self.assertEqual(sd.classify_vote("I accept this proposal."), "accept")
        self.assertEqual(sd.classify_vote("LGTM"), "accept")
        self.assertEqual(sd.classify_vote("+1 from me"), "accept")
        self.assertEqual(sd.classify_vote(" +1"), "accept")  # the boundary-bug regression

    def test_accept_inflections(self):
        self.assertEqual(sd.classify_vote("I'm accepting this."), "accept")
        self.assertEqual(sd.classify_vote("I have accepted it."), "accept")
        self.assertEqual(sd.classify_vote("This is acceptable."), "accept")
        self.assertEqual(sd.classify_vote("approved."), "accept")
        self.assertEqual(sd.classify_vote("agreed."), "accept")

    def test_accept_phrases(self):
        self.assertEqual(sd.classify_vote("I support this change."), "accept")
        self.assertEqual(sd.classify_vote("I will support it."), "accept")
        self.assertEqual(sd.classify_vote("I too support this."), "accept")
        self.assertEqual(sd.classify_vote("I'm in favor."), "accept")
        self.assertEqual(sd.classify_vote("In support."), "accept")
        self.assertEqual(sd.classify_vote("I have no objections."), "accept")
        self.assertEqual(sd.classify_vote("Sounds good to me."), "accept")
        self.assertEqual(sd.classify_vote("I'm fine with this."), "accept")
        self.assertEqual(sd.classify_vote("I'm happy to see this."), "accept")

    def test_reject_basic(self):
        self.assertEqual(sd.classify_vote("I reject this."), "reject")
        self.assertEqual(sd.classify_vote("Vote against."), "reject")
        self.assertEqual(sd.classify_vote("-1 from me"), "reject")
        self.assertEqual(sd.classify_vote("I oppose this."), "reject")

    def test_recuse(self):
        self.assertEqual(sd.classify_vote("I recuse myself."), "recuse")
        self.assertEqual(sd.classify_vote("I'll abstain on this."), "recuse")

    def test_concern(self):
        self.assertEqual(sd.classify_vote("I have concerns."), "concern")
        self.assertEqual(sd.classify_vote("I'm concerned about X."), "concern")
        self.assertEqual(sd.classify_vote("I have reservations."), "concern")

    def test_unclear(self):
        self.assertEqual(sd.classify_vote("Just asking a question here."), "unclear")
        self.assertEqual(sd.classify_vote(""), "unclear")
        self.assertEqual(sd.classify_vote(None), "unclear")

    def test_count_resolution(self):
        # accept appears twice, reject once → accept wins by count
        body = "I recommend we accept this. Otherwise the test would be rejected. Accept!"
        self.assertEqual(sd.classify_vote(body), "accept")

    def test_recuse_priority(self):
        # recuse should win even if other keywords appear
        body = "I'll recuse on this since I am the author. I'd otherwise accept."
        self.assertEqual(sd.classify_vote(body), "recuse")

    def test_tied_accept_reject_unclear(self):
        # Equal accept/reject counts → unclear
        body = "I accept the premise but reject the implementation."
        self.assertEqual(sd.classify_vote(body), "unclear")


class TestFormatVoteSummary(unittest.TestCase):

    def test_empty(self):
        self.assertEqual(sd.format_vote_summary({}), "")

    def test_all_kinds(self):
        votes = {"A": "accept", "B": "accept", "C": "reject", "D": "concern", "E": "recuse"}
        self.assertEqual(sd.format_vote_summary(votes), "2A 1R 1c 1x")

    def test_single_kind(self):
        votes = {"A": "accept", "B": "accept", "C": "accept"}
        self.assertEqual(sd.format_vote_summary(votes), "3A")


class TestCleanSubject(unittest.TestCase):

    def test_strips_list_prefix(self):
        self.assertEqual(
            sd.clean_subject("[ghc-steering-committee] Re: Foo bar"),
            "Foo bar",
        )

    def test_strips_re_chain(self):
        self.assertEqual(
            sd.clean_subject("Re: Re: Re: Foo"),
            "Foo",
        )

    def test_collapses_newlines(self):
        self.assertEqual(
            sd.clean_subject("Long subject\n with continuation"),
            "Long subject with continuation",
        )


if __name__ == "__main__":
    unittest.main()

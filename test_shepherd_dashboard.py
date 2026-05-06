"""Unit tests for shepherd_dashboard.py pure functions."""

import unittest

import shepherd_dashboard as sd


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


class TestCleanSubject(unittest.TestCase):

    def test_strips_list_prefix(self):
        self.assertEqual(
            sd._clean_subject("[ghc-steering-committee] Re: Foo bar"),
            "Foo bar",
        )

    def test_strips_re_chain(self):
        self.assertEqual(
            sd._clean_subject("Re: Re: Re: Foo"),
            "Foo",
        )

    def test_collapses_newlines(self):
        self.assertEqual(
            sd._clean_subject("Long subject\n with continuation"),
            "Long subject with continuation",
        )


if __name__ == "__main__":
    unittest.main()

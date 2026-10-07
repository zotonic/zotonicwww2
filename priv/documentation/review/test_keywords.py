"""Guard keyword coverage, canonical identities, and conversion aliases."""
from copy import deepcopy
import json
from pathlib import Path
import sys
import unittest

DOCS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(DOCS))
import keyword_plan


class KeywordsTest(unittest.TestCase):
    def test_baseline_and_authored_plan(self):
        plan = keyword_plan.build_plan()
        names = {r['name'] for r in plan['resources']}
        self.assertEqual(len(names), len(plan['resources']))
        self.assertIn('doc_cookbook_custom_model', names)
        self.assertNotIn('developer_local_environment', names)
        page = next(r for r in plan['resources'] if r['name'] == 'doc_developerguide_getting_started')
        authored = json.loads((DOCS / 'developer/manifest.json').read_text())['resources']
        source = next(r for r in authored if r['name'] == 'developer_local_environment')
        self.assertEqual(page['zotonic_keywords'], source['zotonic_keywords'])
        self.assertEqual(len(plan['edges']), len({(e['subject'], e['objects'][0]) for e in plan['edges']}))
        for edge in plan['edges']:
            self.assertEqual(edge['predicate'], 'subject')
            self.assertTrue(edge['objects'][0].startswith('zotonic_topic_'))

    def test_unknown_duplicate_and_missing_keywords(self):
        topics = keyword_plan.taxonomy()
        base = dict(name='example', zotonic_keywords=['how_to_guide', 'content_editor', 'surveys'])
        keyword_plan.validate_keywords(base, topics)
        for keywords in ([], ['how_to_guide', 'content_editor'], ['invented_topic'],
                         base['zotonic_keywords'] + ['surveys']):
            page = deepcopy(base)
            page['zotonic_keywords'] = keywords
            with self.assertRaises(AssertionError):
                keyword_plan.validate_keywords(page, topics)


if __name__ == '__main__':
    unittest.main()

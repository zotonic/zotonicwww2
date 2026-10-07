"""Validate the committed portable plan without writing to a destination site."""
import hashlib
import json
from pathlib import Path
import unittest
import sys

DOCS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(DOCS))
from build_import_plan import embed_screenshots, without_repeated_summary


class ImportPlanTests(unittest.TestCase):
    def test_repeated_summary_is_removed_only_at_start(self):
        self.assertEqual('<p>Next step.</p>', without_repeated_summary(
            '<p>Build &amp; deploy.</p>\n<p>Next step.</p>', 'Build & deploy.'))
        for body in ('<p>Build and deploy. Then verify.</p>',
                     '<p><a href="/guide">Build and deploy.</a></p>',
                     '<p>First step.</p><p>Build and deploy.</p>'):
            self.assertEqual(body, without_repeated_summary(body, 'Build and deploy.'))

    @classmethod
    def setUpClass(cls):
        cls.plan = json.loads((DOCS / 'import/plan.json').read_text())

    def test_native_caption_and_alt(self):
        body = '<p><img src="asset://example" alt="A &amp; B"></p>\n<p>A &amp; B</p>\n<p>Next step.</p>'
        result = embed_screenshots(body)
        self.assertIn('<!-- z-media asset://example ', result)
        self.assertIn('"caption": "A & B"', result)
        self.assertIn('"alt": "A & B"', result)
        self.assertNotIn('<p>A &amp; B</p>', result)
        self.assertIn('<p>Next step.</p>', result)

    def test_image_without_caption_keeps_following_prose(self):
        result = embed_screenshots('<p><img src="asset://example" alt="Example"></p>\n<p>Keep this instruction.</p>')
        self.assertIn('"caption": "-"', result)
        self.assertIn('<p>Keep this instruction.</p>', result)
        safe = embed_screenshots('<p><img src="asset://example" alt="--&gt;"></p>')
        self.assertEqual(1, safe.count('-->'))

    def test_deployed_inputs_match_plan(self):
        for item in self.plan['inputs']:
            path = DOCS / item['file']
            self.assertTrue(path.resolve().is_relative_to(DOCS.resolve()))
            self.assertEqual(hashlib.sha256(path.read_bytes()).hexdigest(), item['sha256'], item['file'])
        for row in self.plan['media']:
            if 'file' in row:
                self.assertEqual(hashlib.sha256((DOCS / row['file']).read_bytes()).hexdigest(), row['sha256'])

    def test_single_identity_after_adoption(self):
        rows = self.plan['resources'] + self.plan['media']
        names = [r['name'] for r in rows]
        self.assertEqual(len(names), len(set(names)))
        for alias, target in self.plan['aliases'].items():
            self.assertIn(target, names)
            if alias != target:
                self.assertNotIn(alias, names)
        for row in rows:
            self.assertLessEqual(set(row['properties']), {'title', 'summary', 'body'})
            self.assertTrue(all(isinstance(v, str) for v in row['properties'].values()))

    def test_connections_are_canonical_and_unique(self):
        groups = self.plan['edges']
        self.assertEqual(len(groups), len({(e['subject'], e['predicate']) for e in groups}))
        aliases = self.plan['aliases']
        for edge in groups:
            self.assertEqual(len(edge['objects']), len(set(edge['objects'])))
            self.assertNotIn(edge['subject'], edge['objects'])
            for name in [edge['subject']] + edge['objects']:
                self.assertEqual(aliases.get(name, name), name)
        start = next(e for e in groups if e['subject'] == 'page_start' and e['predicate'] == 'haspart')
        for root in ('editor_guide', 'developer_guide', 'admin_guide'):
            self.assertIn(aliases.get(root, root), start['objects'])

    def test_guide_roots_follow_explicit_sequence(self):
        aliases = self.plan['aliases']
        for bundle in ('editor', 'developer', 'administration'):
            manifest = json.loads((DOCS / bundle / 'manifest.json').read_text())
            root = manifest['root']
            source = sorted((e for e in manifest['edges']
                             if e['subject'] == root and e['predicate'] == 'haspart'),
                            key=lambda e: e['sequence'])
            expected = [aliases.get(n, n) for e in source for n in e['objects']]
            actual = next(e['objects'] for e in self.plan['edges']
                          if e['subject'] == aliases.get(root, root) and e['predicate'] == 'haspart')
            self.assertEqual(expected, actual[:len(expected)])

    def test_reviewed_cookbook_replaces_historical_body(self):
        row = next(r for r in self.plan['resources'] if r['name'] == 'doc_cookbook_frontend_contactform')
        self.assertEqual('preserve', row['existing'])
        self.assertIn('z_email', row['properties']['body'])
        self.assertNotIn('/id/doc_module_mod_email"', row['properties']['body'])
        self.assertTrue(row['source_uri'].startswith('https://zotonic.com/id/'))


if __name__ == '__main__':
    unittest.main()

"""Regression checks for baseline coverage, aliases and non-destructive import data."""
import importlib.util
import json
from pathlib import Path
import unittest
from unittest.mock import patch

DOCS = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('baseline_prepare', DOCS / 'baseline/prepare.py')
baseline = importlib.util.module_from_spec(spec)
spec.loader.exec_module(baseline)


class BaselineRevisionsTest(unittest.TestCase):
    def test_every_page_reviewed(self):
        _, rows, jobs, _, aliases = baseline.build()
        self.assertEqual(94, len(rows))
        self.assertEqual(90, sum(r['action'] == 'replace' for r in rows))
        self.assertEqual(len(rows), len({r['name'] for r in rows}))
        self.assertEqual('doc_cookbook_security_templates_xss', aliases['developer_template_values'])
        self.assertTrue(jobs)

    def test_changed_snapshot_rejected(self):
        with patch.object(baseline, 'sha', return_value='changed'):
            with self.assertRaisesRegex(AssertionError, 'Changed snapshot'):
                baseline.build()

    def test_adopted_content_cannot_be_silently_discarded(self):
        path = DOCS / 'baseline/revisions/manifest.json'
        original = Path.read_text
        def read(p, *args, **kwargs):
            text = original(p, *args, **kwargs)
            if p == path:
                data = json.loads(text)
                row = next(r for r in data['resources'] if r['target_name'] == 'doc_developerguide_directory_structure')
                row['sources'] = row['sources'][:1]
                return json.dumps(data)
            return text
        with patch.object(Path, 'read_text', read):
            with self.assertRaisesRegex(AssertionError, 'Adopted source missing'):
                baseline.build()

    def test_preserved_identity_and_additive_connections(self):
        policy, rows, _, _, _ = baseline.build()
        self.assertEqual(['title', 'summary', 'body'], policy['policy']['properties'])
        for row in rows:
            self.assertLessEqual(set(row['properties']), {'title', 'summary'})
            self.assertNotIn(row['name'], row['hasreference'])
            self.assertEqual(len(row['hasreference']), len(set(row['hasreference'])))
        script = (DOCS / 'baseline/apply-local.erl').read_text()
        self.assertIn('preflight_failed', script)
        self.assertNotIn('z_acl:sudo', script)
        self.assertNotIn('m_edge:set_sequence', script)

    def test_existing_installation_replacements_still_used(self):
        legacy = json.loads((DOCS / 'baseline/revisions/installation.json').read_text())
        reviewed = json.loads((DOCS / 'baseline/revisions/manifest.json').read_text())
        targets = {r['target_name']: r for r in reviewed['resources']}
        for row in legacy['resources']:
            self.assertIn(row['source_file'], [s['file'] for s in targets[row['target_name']]['sources']])

    def test_evidence_paths_and_socket_and_shell_instructions(self):
        root = next(p for p in DOCS.parents if (p / 'apps/zotonic_core').is_dir())
        reviewed = json.loads((DOCS / 'baseline/revisions/manifest.json').read_text())
        for row in reviewed['resources']:
            for source in row['evidence']:
                self.assertTrue((root / source).exists(), source)
        setup = (DOCS / 'developer/01-getting-started/local-postgresql.md').read_text()
        self.assertIn('{dbhost, socket}', setup)
        self.assertIn('local zotonic,postgres zotonic scram-sha-256', setup)
        for bundle in ('developer', 'administration'):
            for path in (DOCS / bundle).rglob('*.md'):
                self.assertNotIn('Ctrl-D', path.read_text(), path)


if __name__ == '__main__':
    unittest.main()

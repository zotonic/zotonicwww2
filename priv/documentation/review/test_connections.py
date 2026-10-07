"""Check graph failures that could lose navigation during an import.

Run: python3 -m unittest discover -s priv/documentation/review -p 'test_*.py'
from the zotonicwww2 application directory.
"""
from copy import deepcopy
import contextlib
import io
import json
from pathlib import Path
import sys
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
import prepare_common


class ConnectionsTest(unittest.TestCase):
    def setUp(self):
        prepare_common.ROOT = ROOT / 'editor'
        self.manifest = json.loads((prepare_common.ROOT / 'manifest.json').read_text())

    def check(self, manifest):
        with contextlib.redirect_stdout(io.StringIO()):
            prepare_common.validate(manifest)

    def test_all_bundles_and_cross_guide_targets(self):
        for bundle in prepare_common.BUNDLES:
            prepare_common.ROOT = ROOT / bundle
            self.check(json.loads((prepare_common.ROOT / 'manifest.json').read_text()))

    def test_unknown_target_rejected(self):
        self.manifest['edges'][-1]['objects'] = ['editor_missing_task']
        with self.assertRaisesRegex(AssertionError, 'Unknown target'):
            self.check(self.manifest)

    def test_generic_or_other_guide_category_rejected(self):
        resource = next(r for r in self.manifest['resources'] if r['category'] == 'userguide')
        for category in ('text', 'developerguide', 'adminguide'):
            resource['category'] = category
            with self.assertRaisesRegex(AssertionError, 'Wrong guide category'):
                self.check(self.manifest)

    def test_shared_task_allowed_but_external_collection_rejected(self):
        edge = next(e for e in self.manifest['edges'] if e['predicate'] == 'haspart')
        extra = deepcopy(edge)
        extra['sequence'] = 1 + sum(e['subject'] == edge['subject'] and e['predicate'] == 'haspart'
                                   for e in self.manifest['edges'])
        extra['objects'] = ['developer_local_environment']
        self.manifest['edges'].append(extra)
        self.manifest['external_resources'].append('developer_local_environment')
        self.check(self.manifest)
        extra['objects'] = ['developer_guide']
        self.manifest['external_resources'][-1] = 'developer_guide'
        with self.assertRaisesRegex(AssertionError, 'External collection members'):
            self.check(self.manifest)

    def test_duplicate_related_target_rejected(self):
        edge = deepcopy(next(e for e in self.manifest['edges'] if e['predicate'] == 'relation'))
        edge['sequence'] = 1 + sum(e['subject'] == edge['subject'] and e['predicate'] == 'relation'
                                   for e in self.manifest['edges'])
        self.manifest['edges'].append(edge)
        with self.assertRaises(AssertionError):
            self.check(self.manifest)

    def test_collection_cycle_rejected_but_related_cycles_allowed(self):
        # Both real bundles contain reciprocal related-task edges and validate.
        self.check(self.manifest)
        edge = next(e for e in self.manifest['edges'] if e['subject'] != self.manifest['root']
                    and e['predicate'] == 'haspart')
        edge['objects'] = [self.manifest['root']]
        with self.assertRaisesRegex(AssertionError, 'Collection cycle'):
            self.check(self.manifest)

    def test_duplicate_sequence_rejected(self):
        group = [e for e in self.manifest['edges']
                 if e['predicate'] == 'haspart' and e['subject'] == self.manifest['root']]
        group[1]['sequence'] = group[0]['sequence']
        with self.assertRaises(AssertionError):
            self.check(self.manifest)


if __name__ == '__main__':
    unittest.main()

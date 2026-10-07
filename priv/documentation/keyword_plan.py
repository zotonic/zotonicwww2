#!/usr/bin/env python3
"""Validate controlled keywords and prepare additive subject connections offline."""
import csv
import hashlib
import json
from pathlib import Path

DOCS = Path(__file__).resolve().parent
BUNDLES = ('editor', 'developer', 'administration')


def taxonomy():
    workspace = next(p for p in DOCS.parents if (p / 'doc/zotonic_subject_topics.csv').is_file())
    with (workspace / 'doc/zotonic_subject_topics.csv').open() as source:
        return {r['keyword_slug']: r for r in csv.DictReader(source)}


def validate_keywords(resource, topics):
    keywords = resource.get('zotonic_keywords')
    assert isinstance(keywords, list) and keywords, f"Missing keywords: {resource['name']}"
    assert all(isinstance(k, str) for k in keywords), f"Invalid keyword: {resource['name']}"
    assert len(keywords) == len(set(keywords)), f"Duplicate keywords: {resource['name']}"
    assert set(keywords) <= topics.keys(), f"Unknown keywords: {resource['name']}: {set(keywords) - topics.keys()}"
    facets = {topics[k]['facet'] for k in keywords}
    assert {'information_type', 'audience'} <= facets, f"Missing keyword facets: {resource['name']}"
    assert facets - {'information_type', 'audience'}, f"Missing subject keyword: {resource['name']}"


def subject_edges(resources):
    return [dict(subject=r['name'], predicate='subject', objects=['zotonic_topic_' + k], sequence=i)
            for r in resources for i, k in enumerate(r['zotonic_keywords'], 1)]


def build_plan():
    topics = taxonomy()
    baseline = DOCS / 'baseline'
    snapshots = {r['target_name']: r for r in json.loads((baseline / 'manifest.json').read_text())['resources']
                 if r['category'] != 'image'}
    legacy = json.loads((baseline / 'keyword-assignments.json').read_text())['resources']
    assert len(legacy) == len(snapshots) and {r['name'] for r in legacy} == snapshots.keys(), 'Incomplete baseline keywords'
    for r in legacy:
        snapshot = snapshots[r['name']]
        assert r['source_uri'] == snapshot['source_uri'] and r['source_sha256'] == snapshot['sha256']
        assert hashlib.sha256((baseline / snapshot['file']).read_bytes()).hexdigest() == snapshot['sha256']
        validate_keywords(r, topics)
    authored = [r for b in BUNDLES for r in json.loads((DOCS / b / 'manifest.json').read_text())['resources']]
    assert len(authored) == len({r['name'] for r in authored}), 'Duplicate authored resource'
    for r in authored:
        validate_keywords(r, topics)
    # Only concrete adoption mappings are applied. Editorial merge candidates
    # without a selected destination remain separate for later review.
    with (DOCS / 'integration/draft-content-map.csv').open() as source:
        aliases = {r['source_name']: r['proposed_target_name'] for r in csv.DictReader(source)
                   if r['action'] in ('replace_existing_body', 'merge_into_existing', 'adopt_existing_root_or_collection')}
    targets = {}
    for r in legacy:
        targets[r['name']] = dict(name=r['name'], source_names=[r['name']], source_uri=r['source_uri'],
                                  zotonic_keywords=r['zotonic_keywords'], assignment_source='baseline')
    for r in authored:
        name = aliases.get(r['name'], r['name'])
        current = targets.setdefault(name, dict(name=name, source_names=[], zotonic_keywords=[], assignment_source='authored'))
        current['source_names'] = list(dict.fromkeys(current['source_names'] + [r['name']]))
        # Reviewed replacement content defines the new topics of an adopted page.
        if current['assignment_source'] == 'baseline':
            current['zotonic_keywords'] = []
        current['assignment_source'] = 'authored'
        current['zotonic_keywords'] = list(dict.fromkeys(current['zotonic_keywords'] + r['zotonic_keywords']))
    resources = list(targets.values())
    return dict(format='zotonic-documentation-keyword-import-plan-v1',
                policy='Resolve names and source URIs on destination; add missing subject edges; preserve existing edges. No resource-body changes.',
                resources=resources, edges=subject_edges(resources),
                keyword_targets=[dict(slug=k, name='zotonic_topic_' + k, label=topics[k]['label'])
                                 for k in sorted({k for r in resources for k in r['zotonic_keywords']})])


def prepare():
    plan = build_plan()
    out = DOCS / 'prepared'
    out.mkdir(exist_ok=True)
    (out / 'keyword-import-plan.json').write_text(json.dumps(plan, ensure_ascii=False, indent=2) + '\n')
    print(f"Prepared keyword import plan for {len(plan['resources'])} destination resources and {len(plan['edges'])} subject connections.")


if __name__ == '__main__':
    prepare()

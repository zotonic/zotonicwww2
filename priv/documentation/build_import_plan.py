#!/usr/bin/env python3
"""Build the portable input for zotonicwww2_documentation_import (no site writes).

Commit import/plan.json with its source changes. Erlang needs no Python at runtime.
"""
import hashlib
import html
import importlib.util
import json
from pathlib import Path
import re

import prepare_common
import keyword_plan

DOCS = Path(__file__).resolve().parent


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def embed_screenshots(body):
    """Use native media figures; recognize the existing repeated-alt caption convention."""
    pattern = r'<p><img src="asset://(?P<name>[a-zA-Z0-9_]+)" alt="(?P<alt>[^"]*)"\s*/?></p>(?P<caption>\s*<p>(?P=alt)</p>)?'
    def embed(match):
        alt = html.unescape(match['alt'])
        options = dict(size='large', alt=alt, caption=alt if match['caption'] else '-')
        # Keep option text inside the HTML comment even when it contains -->.
        encoded = json.dumps(options, ensure_ascii=False).replace('<', r'\u003c').replace('>', r'\u003e')
        return '<!-- z-media asset://' + match['name'] + ' ' + encoded + ' -->'
    return re.sub(pattern, embed, body)


def build():
    for bundle in prepare_common.BUNDLES:
        prepare_common.ROOT = DOCS / bundle
        manifest = json.loads((prepare_common.ROOT / 'manifest.json').read_text())
        prepare_common.validate(manifest)
        prepare_common.render(manifest)
    spec = importlib.util.spec_from_file_location('baseline_prepare', DOCS / 'baseline/prepare.py')
    baseline = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(baseline)
    revisions = baseline.prepare()
    keywords = keyword_plan.build_plan()
    aliases = revisions['aliases']
    canonical = lambda name: aliases.get(name, name)
    snapshots = json.loads((DOCS / 'baseline/manifest.json').read_text())['resources']
    source_ids = {str(r['source_id']): r['target_name'] for r in snapshots}
    rows, edges, media, inputs = {}, {}, [], set()

    def edge(subject, predicate, target):
        subject, target = canonical(subject), canonical(target)
        if subject != target:
            targets = edges.setdefault((subject, predicate), [])
            if target not in targets:
                targets.append(target)

    def english(value):
        return baseline.english(value)

    for snapshot in snapshots:
        path = DOCS / 'baseline' / snapshot['file']
        inputs.add(path)
        assert digest(path) == snapshot['sha256']
        export = json.loads(path.read_text())
        props = export['resource']
        name = snapshot['target_name']
        row = dict(name=name, source_uri=snapshot['source_uri'], category=snapshot['category'],
                   properties={p: english(props.get(p)) for p in ('title', 'summary', 'body')},
                   page_path=props.get('page_path') or '', existing='preserve')
        if snapshot['category'] == 'image':
            row['url'] = export['medium_url']
            assert row['url'].startswith('https://zotonic.com/')
            media.append(row)
        else:
            rows[name] = row
        # Preserve baseline collection and depiction connections on new copies.
        for predicate in ('haspart', 'depiction'):
            for e in sorted(export.get('edges', {}).get(predicate, {}).get('objects', []), key=lambda e: e['seq']):
                obj = e['object_id']
                target = source_ids.get(str(obj['id'])) or obj.get('name')
                if not target and predicate == 'depiction' and 'image' in obj['is_a']:
                    # Five baseline depictions have only edge metadata in the
                    # snapshot. Their source URI remains their portable identity.
                    assert obj['uri'] == f"https://zotonic.com/id/{obj['id']}"
                    target = 'zotonic_com_media_' + str(obj['id'])
                    source_ids[str(obj['id'])] = target
                    media.append(dict(name=target, category='image', source_uri=obj['uri'],
                        existing='preserve', properties={'title': english(obj.get('title'))},
                        url=f"https://zotonic.com/media/attachment/id/{obj['id']}"))
                assert target, (name, predicate, obj)
                edge(name, predicate, target)
    baseline_edges = edges
    edges = {}
    for bundle in prepare_common.BUNDLES:
        root = DOCS / bundle
        manifest = json.loads((root / 'manifest.json').read_text())
        rendered = {r['name']: r for r in json.loads((root / 'prepared/resources.json').read_text())}
        inputs.add(root / 'manifest.json')
        for r in manifest['resources']:
            inputs.add(root / r['file'])
            name = canonical(r['name'])
            if name not in rows:
                props = rendered[r['name']]
                rows[name] = dict(name=name, category=r['category'], source_uri='', page_path='',
                    existing='owned', properties={p: props[p] for p in ('title', 'summary', 'body')})
        for r in manifest['media']:
            path = root / r['file']
            inputs.add(path)
            media.append(dict(name=r['name'], category='image', source_uri='',
                file=str(path.relative_to(DOCS)), sha256=digest(path), existing='owned',
                properties={'title': r['name'].replace('_', ' ')}))
        for e in sorted(manifest['edges'], key=lambda e: (e['subject'], e['predicate'], e['sequence'])):
            for target in e['objects']:
                edge(e['subject'], e['predicate'], target)
    for r in revisions['resources']:
        row = rows[r['name']]
        if r['action'] == 'replace':
            row['properties'] = r['properties']
        for target in r['hasreference']:
            edge(r['name'], 'hasreference', target)
    for r in keywords['resources']:
        assert r['name'] in rows
        for slug in r['zotonic_keywords']:
            edge(r['name'], 'subject', 'zotonic_topic_' + slug)
    # Existing baseline links remain after the authored navigation, not ahead of it.
    for (subject, predicate), targets in baseline_edges.items():
        for target in targets:
            edge(subject, predicate, target)
    for name in ('editor_guide', 'developer_guide', 'admin_guide'):
        edge('page_start', 'haspart', name)
    for r in rows.values():
        def rewrite(match):
            name = canonical(source_ids.get(match[1], match[1]))
            return '/id/' + name
        r['properties']['body'] = embed_screenshots(re.sub(r'/id/([a-zA-Z0-9_]+)', rewrite, r['properties']['body']))
    inputs |= {DOCS / i['file'] for i in revisions['inputs']}
    inputs |= {Path(__file__).resolve(), DOCS / 'baseline/manifest.json'}
    taxonomy = next(p / 'doc/zotonic_subject_topics.csv' for p in DOCS.parents
                    if (p / 'doc/zotonic_subject_topics.csv').is_file())
    plan = dict(format='zotonic-documentation-import-v1', aliases=aliases,
        resources=list(rows.values()), media=media,
        edges=[dict(subject=s, predicate=p, objects=objects) for (s, p), objects in sorted(edges.items())],
        inputs=[dict(file=str(p.relative_to(DOCS)), sha256=digest(p)) for p in sorted(inputs)],
        taxonomy_sha256=digest(taxonomy),
        keyword_names=[r['name'] for r in keywords['keyword_targets']])
    out = DOCS / 'import'
    out.mkdir(exist_ok=True)
    (out / 'plan.json').write_text(json.dumps(plan, ensure_ascii=False, indent=2) + '\n')
    print(f'Built portable plan: {len(rows)} pages, {len(media)} media, {len(edges)} connection groups.')
    return plan


if __name__ == '__main__':
    build()

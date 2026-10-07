#!/usr/bin/env python3
"""Prepare reviewed baseline updates offline; original exports are never modified."""
import csv
import hashlib
import html
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from urllib.parse import urlsplit

BASE = Path(__file__).resolve().parent
DOCS = BASE.parent
sys.path.insert(0, str(DOCS))
import prepare_common
import keyword_plan


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def english(value):
    return value if isinstance(value, str) else (value or {}).get('tr', {}).get('en', '')


def inventory():
    resources = {}
    media = {}
    manifests = []
    for bundle in prepare_common.BUNDLES:
        path = DOCS / bundle / 'manifest.json'
        manifests.append(path)
        manifest = json.loads(path.read_text())
        for row in manifest['resources']:
            resources[(path.parent / row['file']).resolve()] = row
        for row in manifest['media']:
            media[(path.parent / row['file']).resolve()] = row
    with (DOCS / 'integration/draft-content-map.csv').open() as source:
        aliases = {r['source_name']: r['proposed_target_name'] for r in csv.DictReader(source)
                   if r['action'] in ('replace_existing_body', 'merge_into_existing', 'adopt_existing_root_or_collection')}
    return resources, media, aliases, manifests


def build():
    revision_file = BASE / 'revisions/manifest.json'
    revisions = json.loads(revision_file.read_text())
    assert revisions['format'] == 'zotonic-baseline-revisions-v2'
    snapshots = {r['target_name']: r for r in json.loads((BASE / 'manifest.json').read_text())['resources']}
    pages = {n for n, r in snapshots.items() if r['category'] != 'image'}
    rows = revisions['resources']
    assert len(rows) == len(pages) and {r['target_name'] for r in rows} == pages, 'Incomplete or duplicate review'
    sources, media, aliases, manifests = inventory()
    registered = pages | {r['name'] for r in sources.values()}
    registered |= {r['name'] for r in json.loads((DOCS / 'reference-targets.json').read_text())['resources']}
    inputs = set(manifests + [revision_file, BASE / 'manifest.json', BASE / 'keyword-assignments.json',
                             DOCS / 'integration/draft-content-map.csv', DOCS / 'reference-targets.json', BASE / 'apply-local.erl',
                             Path(__file__).resolve(), DOCS / 'prepare_common.py', DOCS / 'keyword_plan.py'])
    jobs, prepared = [], []
    for entry in rows:
        name = entry['target_name']
        snapshot = snapshots[name]
        snapshot_path = BASE / snapshot['file']
        assert entry['snapshot_sha256'] == snapshot['sha256'] == sha(snapshot_path), f'Changed snapshot: {name}'
        assert entry['source_uri'] == snapshot['source_uri']
        inputs.add(snapshot_path)
        raw = json.loads(snapshot_path.read_text())['resource']
        row = dict(name=name, source_uri=entry['source_uri'], snapshot_sha256=snapshot['sha256'],
                   action=entry['action'], reason=entry['reason'], properties={}, parts=[])
        assert entry['action'] in ('replace', 'retain')
        if entry['action'] == 'replace':
            assert entry['sources'] and entry['title'] and entry['summary']
            row['properties'] = dict(title=entry['title'], summary=entry['summary'])
            for i, spec in enumerate(entry['sources']):
                path = (revision_file.parent / spec['file']).resolve()
                assert path.is_relative_to(DOCS) and path.is_file(), f'Missing revision: {path}'
                inputs.add(path)
                body = path.read_text()
                if spec['front_matter']:
                    meta, body = prepare_common.document(path)
                    assert meta == {k: v for k, v in sources[path].items() if k != 'file'}, path
                    if i:
                        body = '## ' + meta['title'] + '\n\n' + body
                elif spec['format'] == 'markdown':
                    heading = '# ' + entry['title'] + '\n'
                    assert body.startswith(heading), path
                    body = body[len(heading):].lstrip()
                assert body.strip(), path
                if spec['format'] == 'markdown':
                    def rewrite(match):
                        image, label, url = match.groups()
                        if urlsplit(url).scheme or url.startswith(('#', '/')):
                            return match.group(0)
                        target = (path.parent / url).resolve()
                        if image:
                            assert target in media, f'Unregistered image: {target}'
                            return f'![{label}](asset://{media[target]["name"]})'
                        assert target in sources, f'Unregistered link: {target}'
                        target_name = sources[target]['name']
                        return f'[{label}](/id/{aliases.get(target_name, target_name)})'
                    body = prepare_common.LINK.sub(rewrite, body)
                    key = f'{name}-{i}'
                    jobs.append(dict(name=key, markdown=body))
                    row['parts'].append(('markdown', key))
                else:
                    assert spec['format'] == 'html'
                    row['parts'].append(('html', body))
            row['old_anchors'] = sorted(set(re.findall(r'\bid=["\']([^"\']+)', english(raw.get('body')))))
        refs = [aliases.get(n, n) for n in entry.get('hasreference', [])]
        assert set(entry.get('hasreference', [])) <= registered, f'Unknown reference on {name}: {set(entry.get("hasreference", [])) - registered}'
        row['hasreference'] = list(dict.fromkeys(n for n in refs if n != name))
        prepared.append(row)
    # An adopted task must be included in the final text, never silently win by import order.
    for path, authored in sources.items():
        target = aliases.get(authored['name'])
        if target in pages:
            final = next(e for e in rows if e['target_name'] == target)
            used = {(revision_file.parent / s['file']).resolve() for s in final.get('sources', [])}
            assert path in used, f'Adopted source missing from final revision: {authored["name"]} -> {target}'
    return revisions, prepared, jobs, inputs, aliases


def render_jobs(jobs):
    workspace = next(p for p in DOCS.parents if (p / 'apps/zotonic_core').is_dir())
    beams = sorted((workspace / '_build/default/lib').glob('*/ebin'))
    assert (workspace / '_build/default/lib/zotonicwww2/ebin/zotonicwww2_doc_link.beam').is_file(), 'Build zotonicwww2 first'
    with tempfile.TemporaryDirectory(prefix='baseline-docs-') as tmp:
        src, dst = Path(tmp) / 'input.json', Path(tmp) / 'output.json'
        src.write_text(json.dumps(jobs))
        expr = '''[Input, Output] = init:get_plain_arguments(),
            {ok, Bin} = file:read_file(Input),
            Result = [#{name => maps:get(<<"name">>, J),
                        html => zotonicwww2_doc_link:to_html(maps:get(<<"markdown">>, J))}
                      || J <- json:decode(Bin)],
            ok = file:write_file(Output, json:encode(Result)), halt().'''
        subprocess.run(['erl', '-noshell', '-pa', *map(str, beams), '-eval', expr,
                        '-extra', str(src), str(dst)], check=True)
        return {r['name']: r['html'] for r in json.loads(dst.read_text())}


def prepare():
    revisions, rows, jobs, inputs, aliases = build()
    rendered = render_jobs(jobs)
    out = DOCS / 'prepared'
    out.mkdir(exist_ok=True)
    for row in rows:
        parts = row.pop('parts')
        if row['action'] == 'replace':
            body = '\n'.join(rendered[value] if kind == 'markdown' else value for kind, value in parts)
            for old, new in aliases.items():
                body = re.sub(r'(/id/)' + re.escape(old) + r'(?=["\'#?])', lambda m: m[1] + new, body)
            # Preserve old fragment URLs even when a section is reorganized.
            anchors = set(re.findall(r'\bid=["\']([^"\']+)', body))
            missing = [a for a in row.pop('old_anchors') if a not in anchors]
            body = ''.join('<span id="' + html.escape(a, quote=True) + '"></span>' for a in missing) + body
            row['properties']['body'] = body
            # Asset placeholders are resolved by the guide-media import stage.
            row['required_media'] = sorted(set(re.findall(r'asset://([a-z0-9_]+)', body)))
            preview = body.replace('href="/id/', 'href="https://zotonic.com/id/')
            media_by_name = {m['name']: path for path, m in inventory()[1].items()}
            for name in row['required_media']:
                import os
                preview = preview.replace('asset://' + name, os.path.relpath(media_by_name[name], out))
            (out / (row['name'] + '.html')).write_text('<!doctype html><meta charset="utf-8"><title>' +
                html.escape(row['properties']['title']) + '</title><style>body{max-width:56rem;margin:3rem auto;font:18px/1.6 system-ui}pre{overflow:auto}img{max-width:100%}</style><h1>' +
                html.escape(row['properties']['title']) + '</h1>' + preview)
    keywords = keyword_plan.build_plan()
    keyword_rows = {r['name']: r for r in keywords['resources']}
    for row in rows:
        row['subject'] = ['zotonic_topic_' + k for k in keyword_rows[row['name']]['zotonic_keywords']]
    plan = dict(format='zotonic-baseline-import-plan-v2', language='en', policy=revisions['policy'],
                aliases=aliases, inputs=[dict(file=str(p.relative_to(DOCS)), sha256=sha(p)) for p in sorted(inputs)],
                resources=rows, keyword_targets=keywords['keyword_targets'])
    (out / 'baseline-import-plan.json').write_text(json.dumps(plan, ensure_ascii=False, indent=2) + '\n')
    print(f'Prepared {len(rows)} reviewed baseline pages: {sum(r["action"] == "replace" for r in rows)} body replacements, with references and keywords.')
    return plan


if __name__ == '__main__':
    prepare()

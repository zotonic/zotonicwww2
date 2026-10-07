#!/usr/bin/env python3
"""Validate a documentation guide and optionally render it with Zotonic's Markdown plugin.

This is an offline preparation tool. It never connects to or changes a website.
Markdown files and manifest.json are the editable sources; prepared/ is disposable.
"""

import argparse
from collections import defaultdict
import html
import json
from pathlib import Path
import re
import subprocess
import tempfile
from urllib.parse import urlsplit
import keyword_plan

ROOT = None
DOCS = Path(__file__).resolve().parent
BUNDLES = ("editor", "developer", "administration")
GUIDE_CATEGORIES = {
    "editor": "userguide",
    "developer": "developerguide",
    "administration": "adminguide",
}
LINK = re.compile(r"(!?)\[([^\]]*)\]\(([^)\s]+)\)")


def document(path):
    """Read the documented JSON-valued subset of YAML front matter."""
    text = path.read_text(encoding="utf-8")
    assert text.startswith("---\n"), f"Missing front matter: {path}"
    front, body = text[4:].split("\n---\n", 1)
    metadata = {}
    for line in front.splitlines():
        key, value = line.split(":", 1)
        assert key not in metadata, f"Duplicate property: {path}: {key}"
        metadata[key] = json.loads(value)
    heading = "# " + metadata["title"] + "\n"
    body = body.lstrip()
    assert body.startswith(heading), f"Title mismatch: {path}"
    return metadata, body[len(heading):].lstrip()


def validate(manifest):
    resources = manifest["resources"]
    topics = keyword_plan.taxonomy()
    names = {r["name"] for r in resources}
    assert len(names) == len(resources), "Duplicate resource names"
    assert all(re.fullmatch(r"(?:editor|developer|admin)_[a-z0-9_]+", n) and len(n) <= 80 for n in names)
    files = {r["file"] for r in resources}
    assert len(files) == len(resources), "Duplicate source files"
    media_files = {m["file"] for m in manifest["media"]}
    assert len(media_files) == len(manifest["media"]), "Duplicate media files"
    media_names = {m["name"] for m in manifest["media"]}
    assert len(media_names) == len(manifest["media"]) and not media_names & names
    used_media = set()
    for r in resources:
        keyword_plan.validate_keywords(r, topics)
        assert r["category"] in (GUIDE_CATEGORIES[ROOT.name], "collection"), \
            f"Wrong guide category: {r['name']}: {r['category']}"
        meta, body = document(ROOT / r["file"])
        assert meta == {k:v for k,v in r.items() if k != "file"}, f"Manifest mismatch: {r['file']}"
        assert body.strip(), f"Empty page: {r['file']}"
        for image, alt, url in LINK.findall(body):
            if urlsplit(url).scheme or url.startswith("#"):
                continue
            target = ((ROOT / r["file"]).parent / url).resolve()
            assert target.is_relative_to(ROOT), f"Link outside bundle: {url}"
            relative = target.relative_to(ROOT).as_posix()
            assert target.is_file(), f"Broken link: {r['file']} -> {url}"
            if image:
                assert alt.strip(), f"Missing image description: {r['file']}"
                assert relative in media_files, f"Unregistered image: {relative}"
                used_media.add(relative)
            else:
                assert relative in files, f"Link to non-content file: {relative}"
    assert used_media == media_files, f"Unused images: {media_files - used_media}"
    for m in manifest["media"]:
        assert (ROOT / m["file"]).stat().st_size > 1000, f"Empty screenshot: {m['file']}"
    external = set(manifest.get("external_resources", []))
    registry = {r["name"] for r in json.loads((DOCS/"reference-targets.json").read_text())["resources"]}
    sibling_resources = {r["name"]: r for path in DOCS.glob("*/manifest.json")
               if path.parent.name in BUNDLES
               for r in json.loads(path.read_text())["resources"]}
    sibling = set(sibling_resources)
    assert external <= registry | sibling, f"Unregistered external targets: {external - registry - sibling}"
    assert not external & names, "Local target declared external"
    assert len(external) == len(manifest.get("external_resources", [])), "Duplicate external target"
    predicates = {p["name"] for p in manifest.get("predicates", [])}
    assert "hasreference" in predicates, "Missing hasreference predicate declaration"
    all_groups = defaultdict(list)
    for e in manifest["edges"]:
        assert e["subject"] in names, f"Unknown subject: {e}"
        assert e["predicate"] in ("haspart", "relation", "hasreference"), f"Unknown predicate: {e}"
        assert len(e["objects"]) == 1 and e["objects"][0] in names | external, f"Unknown target: {e}"
        assert e["objects"][0] != e["subject"], f"Self connection: {e}"
        assert type(e["sequence"]) is int, f"Invalid sequence: {e}"
        if e["predicate"] == "haspart":
            target = e["objects"][0]
            # Reuse authored tasks across guides, but keep collection trees local.
            assert target in names or sibling_resources.get(target, {}).get("category") in GUIDE_CATEGORIES.values(), \
                "External collection members must be authored guide pages"
        all_groups[e["subject"], e["predicate"]].append((e["sequence"], e["objects"][0]))
    for key, members in all_groups.items():
        assert sorted(n for n, _ in members) == list(range(1, len(members)+1)), key
        assert len({name for _, name in members}) == len(members), key
    assert external == {e["objects"][0] for e in manifest["edges"] if e["objects"][0] not in names}, "Unused external targets"
    groups = {subject: members for (subject, predicate), members in all_groups.items() if predicate == "haspart"}
    collections = {r["name"] for r in resources if r["category"] == "collection"}
    assert set(groups) == collections, "Missing or unexpected collection edges"
    for parent, members in groups.items():
        assert sorted(n for n,_ in members) == list(range(1,len(members)+1)), parent
        assert len({name for _,name in members}) == len(members), parent
    visited = set()

    def visit(name, ancestors):
        assert name not in ancestors, f"Collection cycle: {name}"
        visited.add(name)
        for _, child in groups.get(name, []):
            visit(child, ancestors | {name})

    visit(manifest["root"], set())
    assert visited & names == names, f"Unreachable pages: {names - visited}"
    print(f"Validated {len(resources)} resources, {len(manifest['edges'])} ordered edges, "
          f"and {len(media_files)} screenshots; all local links resolve.")


def render(manifest):
    """Render locally with the site's actual compiled Markdown extension."""
    topics = keyword_plan.taxonomy()
    workspace = next(
        parent for parent in ROOT.parents
        if (parent / "apps/zotonic_core").is_dir() and (parent / "rebar.config").is_file()
    )
    beams = sorted((workspace / "_build/default/lib").glob("*/ebin"))
    renderer = workspace / "_build/default/lib/zotonicwww2/ebin/zotonicwww2_doc_link.beam"
    assert renderer.is_file(), "Build zotonicwww2 before rendering."
    out = ROOT / "prepared"
    out.mkdir(exist_ok=True)
    by_file = {r["file"]:r for r in manifest["resources"]}
    prepared = []
    jobs = []
    for r in manifest["resources"]:
        _, body = document(ROOT / r["file"])

        def rewrite(match):
            image, label, url = match.groups()
            if urlsplit(url).scheme or url.startswith("#"):
                return match.group(0)
            target = ((ROOT/r["file"]).parent/url).resolve().relative_to(ROOT).as_posix()
            if image:
                # Remains explicitly unresolved until the media upload returns a URL.
                media = next(m for m in manifest["media"] if m["file"] == target)
                return f"![{label}](asset://{media['name']})"
            return f"[{label}](/id/{by_file[target]['name']})"

        body = LINK.sub(rewrite, body)
        jobs.append({"name":r["name"], "markdown":body})
    # A fresh local VM only loads the Markdown renderer. No server RPC or credentials.
    with tempfile.TemporaryDirectory(prefix="editor-docs-") as temp:
        input_path = Path(temp)/"input.json"
        output_path = Path(temp)/"output.json"
        input_path.write_text(json.dumps(jobs), encoding="utf-8")
        expression = '''
            [Input, Output] = init:get_plain_arguments(),
            {ok, Data} = file:read_file(Input),
            Results = [#{name => maps:get(<<"name">>, J),
                         html => zotonicwww2_doc_link:to_html(maps:get(<<"markdown">>, J))}
                       || J <- json:decode(Data)],
            ok = file:write_file(Output, json:encode(Results)), halt().
        '''
        subprocess.run(["erl", "-noshell", "-pa", *map(str,beams), "-eval", expression,
                        "-extra", str(input_path), str(output_path)], check=True)
        rendered = {d["name"]:d["html"] for d in json.loads(output_path.read_text())}
    for r in manifest["resources"]:
        prepared.append({"name":r["name"], "category":r["category"],
                         "language":["en"], "title":r["title"], "summary":r["summary"],
                         "is_published":r["is_published"], "body":rendered[r["name"]]})
    (out/"resources.json").write_text(json.dumps(prepared,ensure_ascii=False,indent=2)+"\n")
    (out/"edges.json").write_text(json.dumps(
        manifest["edges"] + keyword_plan.subject_edges(manifest["resources"]),
        ensure_ascii=False, indent=2)+"\n")
    targets = {r["name"]: r for r in json.loads((DOCS/"reference-targets.json").read_text())["resources"]}
    for path in DOCS.glob("*/manifest.json"):
        if path.parent.name in BUNDLES:
            for target in json.loads(path.read_text())["resources"]:
                targets[target["name"]] = {**target, "url": "../../"+path.parent.name+"/prepared/"+target["name"]+".html"}
    for r in prepared:
        body = r["body"]
        for predicate, heading in [("haspart", "In this collection"), ("relation", "Related tasks"), ("hasreference", "Further reading")]:
            connections = sorted((e for e in manifest["edges"]
                                  if e["subject"] == r["name"] and e["predicate"] == predicate),
                                 key=lambda e:e["sequence"])
            if connections:
                body += "<section><h2>"+heading+"</h2><ul>"
                for edge in connections:
                    target = targets[edge["objects"][0]]
                    body += '<li><a href="'+html.escape(target["url"], quote=True)+'">'+html.escape(target["title"])+"</a></li>"
                body += "</ul></section>"
        source = next(source for source in manifest["resources"] if source["name"] == r["name"])
        body += '<section><h2>Keywords</h2><ul>'
        for slug in source['zotonic_keywords']:
            body += '<li><a href="https://zotonic.com/id/zotonic_topic_' + slug + '">' + html.escape(topics[slug]['label']) + '</a></li>'
        body += '</ul></section>'
        for m in manifest["media"]:
            body = body.replace("asset://"+m["name"], "../"+m["file"])
        for target in prepared:
            body = body.replace('href="/id/'+target["name"]+'"', 'href="'+target["name"]+'.html"')
        # The extension's developer-reference links should open the real reference site.
        body = body.replace('href="/id/', 'href="https://zotonic.com/id/')
        title = html.escape(r["title"])
        preview = ('<!doctype html><html lang="en"><meta charset="utf-8">'
                   '<meta name="viewport" content="width=device-width, initial-scale=1">'
                   '<title>'+title+'</title><style>body{font:18px/1.6 system-ui,sans-serif;'
                   'max-width:850px;margin:3rem auto;padding:0 1.2rem;color:#243128}'
                   'img{max-width:100%;height:auto;border:1px solid #ddd}'
                   'a{color:#286b42}h1,h2{line-height:1.2}li{margin:.4rem 0}'
                   'pre{overflow:auto;background:#f4f6f4;padding:1rem;font-size:.85em}'
                   'table{border-collapse:collapse}td,th{padding:.4rem;border:1px solid #ddd}'
                   '</style><nav><a href="'+manifest['root']+'.html">Guide contents</a></nav>'
                   '<main><h1>'+title+'</h1>'+body+'</main></html>')
        (out/(r["name"]+".html")).write_text(preview,encoding="utf-8")
    print(f"Rendered {len(prepared)} resources with zotonicwww2_doc_link; "
          f"prepared/{manifest['root']}.html is the local preview.")


def main(root):
    global ROOT
    ROOT = Path(root).resolve()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--render", action="store_true", help="also prepare HTML with the site's renderer")
    args = parser.parse_args()
    manifest = json.loads((ROOT/"manifest.json").read_text(encoding="utf-8"))
    validate(manifest)
    if args.render:
        render(manifest)
        keyword_plan.prepare()
        # Baseline corrections have final precedence over adopted guide bodies.
        import importlib.util
        spec = importlib.util.spec_from_file_location("baseline_prepare", DOCS / "baseline/prepare.py")
        baseline_prepare = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(baseline_prepare)
        baseline_prepare.prepare()

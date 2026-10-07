# Developer documentation staging bundle

This directory contains English developer documentation for later import into zotonic.com: 147 text pages, grouped into 12 topic collections, with one root collection. Nothing has been imported or published.

Start at [the guide](index.md) or open `prepared/developer_guide.html` for the rendered local preview.

## Contents

- `01-getting-started`: environment, first site, first page, development cycle.
- `02-applications`: application identities, file layout, discovery, priority, lifecycle, storage.
- `03-command-line-workflows`: everyday terminal and Erlang shell tasks.
- `04-templates`: layouts, components, overrides, URLs, values, images, translations, caches, assets.
- `05-content-data`: contexts, resources, categories, edges, searches, media, fixtures, access, SQL, imports.
- `06-reusable-functionality`: modules, models, controllers, observers, template extensions, schema upgrades, workers.
- `07-browser-apis`: wires, postbacks, forms, messaging, APIs, browser diagnosis.
- `08-development-tools`: Development module settings, template tools, dispatch, observers, database and function tracing, caches, logs.
- `09-testing`: focused Erlang, template, permission, upgrade, and dependency checks.
- `10-configuration-deployment`: configuration layers, environments, storage, release and recovery workflows.
- `11-troubleshooting`: common startup, discovery, routing, model, media, and performance problems.
- `12-command-reference`: one page for each of the 39 commands in the checked launcher source.

The guide recommends **Erlang/OTP 28** and uses a fictional local site, `garden`. Getting started provides an ordered first-run walkthrough; choose one installation route. Commands elsewhere are task-specific examples, not one script to execute in sequence. Complete module and model examples are provided; callback fragments elsewhere are identified as fragments.

## Editing and local preparation

Edit the Markdown files directly. Front matter uses YAML keys with JSON values; this restricted format is parsed without an extra Python dependency. Keep corresponding metadata in `manifest.json` in sync. The `source_paths` fields identify repository sources checked for a topic; they are staging metadata, not site resource properties.

Use ordinary Markdown for text, tables, code, and relative page links. The zotonicwww2 extension links inline reference spans such as `module#mod_development`, `model#rsc`, and `tag#catinclude`. It does not change fenced code examples.

From this directory:

```sh
python3 prepare.py
python3 prepare.py --render
```

Validation needs Python 3. Rendering additionally needs Erlang with `json` available and the built Zotonic workspace, including `zotonicwww2_doc_link`. It runs a separate local VM and makes no server calls. `prepared/` is generated output; edit source Markdown instead. The preview uses local HTML links for this guide and zotonic.com links for existing reference documentation.

## Later import

`manifest.json` records stable names, resource categories, parent/order metadata, and 154 ordered `haspart` edges. New pages, collections and screenshots are published on import for review. No OAuth key is stored here, and the preparation script performs no import.

1. Authenticate to the destination using the OAuth credentials supplied for the import. Check the target site and available permissions.
2. Resolve the intended parent collection and existing resources by stable name. Inspect conflicts before overwriting existing content; do not match numeric IDs between sites.
3. Create or update the collections and text resources using the destination's supported resource API. Publish newly created resources for review; preserve publication state on existing resources. `prepared/resources.json` contains normalized staging properties and HTML; it is **not a ready-to-send API request body**.
4. Record the returned destination IDs. Resolve guide links using that mapping or confirm that the stable `/id/<name>` URLs resolve on the destination.
5. Apply `haspart` relationships in manifest sequence order. Parent/order fields are preparation metadata, not substitutes for those edges.
6. Attach the root collection to the chosen documentation collection, review navigation and references, and review the published new pages.

This developer bundle includes a screenshot of the standard Site Development tools. Its media resource is registered in `manifest.json`. No site-specific dashboard content is embedded here.

See [verification notes](VERIFICATION.md) for what was checked and the limits of that verification.

## Audience review and connected navigation

The [2026-10-07 review](../review/README.md) records page coverage, changes,
and remaining publication work. Collection contents come from `haspart`;
standalone related-task lists now use ordered `relation` edges. Curated reference
links use **`hasreference`**, distinct from `refers` managed by `mod_admin` and the
site's legacy `references` predicate.

Render both bundles to follow cross-guide links in the preview. `prepared/edges.json`
is a copy of the graph for inspection, not an executable API request. Importers
must apply **all three predicates**, resolve external targets and aliases, and
follow the [connection contract](../CONNECTIONS.md). Body HTML deliberately omits
connection lists; importing only bodies would lose their navigation.

## Installation rewrite

[Start the first-run walkthrough](01-getting-started/local-environment.md). The
[baseline revision map](../baseline/revisions/installation.json) identifies the
three existing installation pages these texts replace while preserving their URLs.
See [verification and merge notes](../review/installation-review.md).

Task pages use category `developerguide`; landing and topic collections use
`collection`. Imported reference destinations may retain their existing more
specific category when the reviewed integration map calls for that.

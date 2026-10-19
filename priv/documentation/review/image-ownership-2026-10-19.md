# Documentation image ownership

Audited the core Erlang documentation, Markdown under `doc`, guide sources,
captured baseline documents, and duplicate files in the website image library.

## Module screenshots

Both active screenshot references in the core Erlang documentation now use
module-owned files with paths relative to their Erlang source:

| Documentation | Source image |
| --- | --- |
| `mod_development` | `apps/zotonic_mod_development/priv/doc/admin-development.jpg` |
| `scomp_base_debug` | `apps/zotonic_mod_base/priv/doc/scomp-base-debug-expanded.png` |

The core reference importer resolves these paths, imports named media resources,
and emits native media embeds. These images no longer need copies in `doc/img`
or the website's `priv/lib/images`. Deploy and reimport the reference documents
together with the removal of their old static files.

## Guides and baseline

Guide screenshots already belong to the guide sources in their `assets`
directories and are imported through the documentation import plan. No
`/lib/images/` or `doc/img/` references were found in the captured baseline and
guide Markdown, HTML, or JSON files. Existing native media references remain
unchanged.

## Retained legacy files

Other duplicated screenshots in `doc/img` and the website image library have no
current documentation source references. These include the old development
debugging, admin, status, filestore, MQTT, chart, and datamodel screenshots.
They are retained because older published pages outside the captured baseline
may still reference their static URLs. This source audit does not prove that
they are unused in production.

Branding assets and image URLs inside code examples are not documentation
screenshots. Their runtime paths remain unchanged.

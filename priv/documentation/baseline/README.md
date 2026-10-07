# Existing documentation baseline

Public zotonic.com exports fetched on 2026-10-07 for local conversion tests.
The snapshot contains 94 pages and 2 associated image exports. The page inventory
covers the public User Guide, Developer Guide and Cookbook categories, their
selected guide children, and the command-line reference landing page. It does
not include the full generated reference or release history.

`manifest.json` records source IDs, URIs, names, local matching names, export
paths, and SHA-256 hashes. `resources/` contains the unmodified JSON exports.
Image exports contain download URLs; media binaries are fetched by Zotonic into
local site storage, not committed in this directory.

## Reviewed replacements

The [complete revision manifest](revisions/manifest.json) supplies reviewed texts
for 90 of the 94 captured pages; four bodies remain valid. The three installation
replacements are included. Original snapshots and hashes remain unchanged.

Run `baseline/prepare.py` to produce `prepared/baseline-import-plan.json`. Apply
that plan after seeding and guide adoption so obsolete text cannot win by import
order. [Revision instructions](revisions/README.md) describe preparation, identity
preservation, the local `apply-local.erl` audit/apply routine, and required media
and connection targets. The missing-only seed below remains a raw conversion
fixture; it does not perform the final text revision.

## Identity

Most old documentation pages have stable unique names. Three pages in this
snapshot do not. These explicit local names are used instead:

| Source ID | Source page | Local name |
| --- | --- | --- |
| 2310 | Download Zotonic | `doc_developerguide_download` |
| 2325 | Security, templates and XSS prevention | `doc_cookbook_security_templates_xss` |
| 2410 | CSS classes used in templates | `doc_developerguide_css_classes` |

The two unnamed image resources use `zotonic_com_media_2297` and
`zotonic_com_media_2328`. Imported resources remember their source URI as an alias.
Match by name, then source URI. Never use a production numeric ID as a local ID.
These names were assigned only to local copies; production resources were not
renamed.

The integration inventory was captured a day earlier and only followed selected
collection links. Its null source names remain accurate. Use this baseline's
explicit mapping when resolving unnamed pages during local conversion tests.

## Audit and seed the local site

From the workspace root, connect using `bin/zotonic shell`. In that shell:

```erlang
C = z_acl:sudo(z:c(zotonicwww2)).
Script = filename:join([
    code:priv_dir(zotonicwww2), "documentation", "baseline", "seed-local.erl"
]).
{ok, Audit} = file:script(Script, [{'Context', C}, {'Mode', audit}]).
```

`audit` reads resources and relationships without changing the site. To import
missing baseline resources:

```erlang
{ok, Report} = file:script(Script, [{'Context', C}, {'Mode', seed}]).
```

The script accepts only site `zotonicwww2` in environment `development` or `test`
and requires admin-module permission. It checks snapshot hashes and matches
existing names and source URIs. New resources are independent authoritative
local copies imported through `m_rsc_import`. Existing bodies are never replaced.

All resource IDs are resolved before connecting pages. The seed fills missing
`haspart` links where the existing list is an ordered subset of the source list.
It preserves extra local edges and locally changed order and reports those
cases. For seeded copies it also restores `depiction` and `subject` links. Equal
source sequence values retain the export's order.

This script is deliberately not a reset command: rerunning it after conversion
will not restore old bodies or silently undo local collection changes. To repeat
an entire destructive conversion, restore a test database/files backup or use a
fresh isolated test site with an explicitly adapted seed routine.

Media downloads use Zotonic's asynchronous import tasks. Verify `m_media:get/2`
and the media file after tasks complete; resource creation alone does not confirm
the image download. A failed media download may be retried by that task system.

Exit the remote shell with Ctrl-C twice, not `q().`.

## Verified local result, 2026-10-07

- 91 of the 94 pages already existed by name and had nonempty bodies. Their
  bodies were preserved, including local edits.
- Imported the three unnamed pages and their two depiction images.
- Restored the Developer Guide's missing CSS-class child in source order; it
  now has all 30 direct children. User Guide retains its four direct children.
- Restored the Download page's three children and both imported pages' depiction
  links, plus the XSS page's eight topic links.
- Both image imports completed with JPEG media records. The site moves the files
  into its configured filestore; both are served successfully as JPEGs.
- A subsequent seed run created no resources and changed no checked relationships.
- The local Cookbook also has seven older entries outside the current live
  inventory: six former index/grouping pages and `doc_cookbook_frontend_mod_chat`.
  They were retained. Use the baseline manifest to select the current source
  set in tests rather than assuming every local Cookbook resource came from
  this snapshot.

`local-seed-report.json`, `local-verify-report.json`, and `local-http-checks.json`
are generated records of
this run, including source-to-local IDs. They are ignored by Git because another
installation will assign different IDs. The checked-in snapshot manifest is the
portable identity source.

## Keywords for conversion

`keyword-assignments.json` supplies controlled keywords for all 94 baseline pages,
including the 45 cookbooks. Apply them during conversion even when a page already
exists. They are an additive overlay, not changes to the original snapshots or
the missing-only seed. See [keyword import rules](../KEYWORDS.md).

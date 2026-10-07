# Documentation connections

The manifests use stable resource names, never source-site numeric IDs. Their
`zotonic-documentation-v2` format adds connections and explicit external targets
to the previous staging format. Resource front matter remains unchanged.

| Predicate | Meaning | Rendering |
| --- | --- | --- |
| `haspart` | Ordered members of a collection | Collection contents and previous/next navigation |
| `relation` | Curated related tasks or concepts | Outgoing related-page list, in edge order |
| `hasreference` | Curated further reading/reference documentation | Outgoing further-reading list, in edge order |
| `subject` | Controlled keywords from `zotonic_keywords` | Keyword navigation and discovery; generated during preparation |
| `refers` | Automatically tracked resource uses in content | Owned by `mod_admin`; do not reconcile from these manifests |

The site's older `references` predicate remains untouched. New guide manifests
use `hasreference`. Zotonicwww2 schema version 26 adds that predicate for text and
collection subjects, with text, collection, and media targets. It is not a new
category and does not replace category `reference`.

Each edge contains `subject`, `predicate`, one name in `objects`, and a one-based
`sequence`. Sequence belongs to a `(subject, predicate)` pair, not to the target
resource globally. The validator rejects duplicate targets, unknown names,
self-links, gaps or duplicate positions, and cycles in `haspart`. Related tasks
may point back to each other; they are not children in the collection tree.

Keep a link in the body when the reader needs it to understand a sentence or
complete a step. Put standalone suggestions in `relation`. Inline reference
spans such as `model#rsc` can remain in explanatory prose and also have a
`hasreference` edge. Collection and related-link lists must not be hand-maintained
again in Markdown; the graph is their source of truth.

## Import in two passes

1. Validate all three manifests. Resolve root aliases and merged-page aliases from
   the approved integration plan before resolving any body links or edges.
2. Resolve categories, predicates, content groups and every existing target by
   **name on the destination**. `external_resources` names targets outside that
   bundle; some belong to another bundle, others are listed in
   `reference-targets.json`. The registry's local verification does not prove
   those names exist on production. Stop and report unresolved targets; do not
   create placeholder copies of reference pages.
3. Create/update the owned resources and media, recording the returned IDs.
   Publish new resources on import for review; preserve publication state on existing resources. Resolve body links and staged media URLs.
4. Resolve and apply the three curated connection types and the keyword `subject` connections after all guides' resources
   exist. A page linked from another guide is reused, not copied. Apply sequence
   explicitly and read back the result.
5. Keep a previous-import ledger of managed resources, properties and edges.
   Reconcile only edges owned by this documentation import. Do not call
   `m_edge:set_sequence/4` with a partial list on an adopted page: it can remove
   other connections. Preserve editorial additions in their relative order after the source-ordered
   connections. The importer reports how many extra edges it preserved. A retry with unchanged inputs should make no changes.
6. Verify actual target titles/URLs, order, anonymous visibility, and an unchanged
   second run. Reference pages keep their own publication state and importer
   ownership; this importer must not unpublish or overwrite them.

Use normal model APIs and destination ACLs. `parent`, `order`, `source_paths`,
`external_resources` and predicate declarations are import metadata, not arbitrary
resource properties to send to `m_rsc`.

## Rendering

The shared offline preparer writes `prepared/resources.json` (bodies only),
`prepared/edges.json` (including generated `subject` edges), and HTML previews with lists derived from the graph.
Cross-guide previews require rendering all three bundles. Reference preview links go
to zotonic.com by stable name; actual site templates use the target's `page_url`.

`priv/templates/_page_documentation_connections.tpl`, included by `page.tpl`,
uses `id.o.relation` and `id.o.hasreference`, filters targets through `is_visible`,
and displays their current titles. Renaming a target therefore updates link text
without rewriting all referring pages. The existing `haspart` rendering supplies
collection navigation. Automatic subject-based suggestions remain separate from
these curated connections.

The [full importer](import/README.md) adopts the mapped guide roots and connects
them to `page_start`. Existing resource categories and URLs are preserved.

## Shared collection members

An administration collection can include an existing editor or developer task
with `haspart`. The shared task remains owned by its original source bundle and
keeps its name; its `parent` metadata is not a demand to remove other collection
memberships. External members must be authored guide pages (`userguide`, `developerguide`, or `adminguide`). Collection-to-
collection edges stay local so validation can detect cycles within each tree.
Resolve shared tasks before applying collection membership during import.

Guide task categories inherit from `documentation` and `text`, so the existing
text-category predicate constraints also apply to these pages. The category
determines page type; `haspart` determines where it appears in the guide tree.

## Keyword assignments

See [KEYWORDS.md](KEYWORDS.md) for the controlled taxonomy, baseline overlay,
and combined import plan. `subject` edges come from `zotonic_keywords`; they
are generated at preparation time rather than duplicated in source edge lists.
Import keywords for existing articles and cookbooks as well as new pages.

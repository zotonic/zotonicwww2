# Controlled documentation keywords

All authored pages and collections have `zotonic_keywords` in their Markdown
front matter and manifest entry. Values are canonical `keyword_slug` identifiers
from the checkout's `doc/zotonic_subject_topics.csv`, not free-text labels.
Assignments include information type, audience, and a small set of relevant
subjects. They do not tag every incidental word in a page.

The site's subject importer maps a slug to a keyword resource named
`zotonic_topic_<slug>`. Connect the article to that resource with **`subject`**.
Do not create a new predicate, use `hasreference` for topics, or manufacture
duplicate keywords from display labels. Keyword resources retain their existing
facet categories and taxonomy relationships.

## New and existing content

- 318 authored resources have assignments in their own source metadata.
- [baseline/keyword-assignments.json](baseline/keyword-assignments.json) covers
  all 94 existing baseline pages, including 45 cookbook entries. It records each
  original source URI and checksum. Original exports and hashes stay unchanged.
- Cookbook placeholders retain a review note: a topic assignment does not make
  unfinished or outdated content ready for publication.
- Shared tasks use one set of keywords, regardless of collection membership.

These existing-page assignments cover the captured baseline, not an unseen
inventory of every article on the live site. Add newly discovered existing
articles to the inventory and review their topics before conversion.

## Prepare the import data

Render a guide as usual, or run this from the workspace root:

```sh
python3 apps_user/zotonicwww2/priv/documentation/keyword_plan.py
```

Each rendered guide's `prepared/edges.json` includes its navigation connections
and generated `subject` edges. The combined
`prepared/keyword-import-plan.json` includes both authored and existing pages.
It resolves concrete adoption mappings from the integration CSV, combines
authored sources targeting one page, and uses reviewed replacement keywords
instead of the old baseline assignment when a page's body is being replaced.
Unresolved editorial merge candidates remain separate until their mapping is
settled. The current proposal produces 383 destination resources and 1,555
subject connections from the 412 source records.

Review keywords through the generated HTML previews or the source metadata.
The preview's keyword list is not embedded into the stored article body.
`zotonic_keywords` is import metadata; materialize its connections rather than
blindly passing it as a resource property.

## Apply during conversion/import

1. Import or verify the controlled taxonomy on the destination first. Resolve
   every `zotonic_topic_*` by name; stop for missing or ambiguous targets.
2. Finalize adoption mappings and regenerate the plan. Resolve article names
   and baseline source URIs to destination IDs, never source numeric IDs.
3. Apply article/body changes from the content plan, then add missing `subject`
   edges from the combined keyword plan. This includes articles and cookbooks
   already present on the destination, not only newly created pages.
4. Preserve existing keyword connections, including legacy keywords outside
   this taxonomy. Do not replace the entire `subject` list or use a partial
   `set_sequence` call. Sequence in this plan describes assignment order, not
   permission to reorder or remove unmanaged connections.
5. Record inserted edges in the documentation import ledger. Verify the
   resulting links and that a second run adds no duplicate edges. Later keyword
   removals need explicit ownership review; the initial plan is additive.

Do not apply both per-guide and combined keyword edges as competing owners.
The combined plan is authoritative for the final keyword pass after aliases are
resolved. Per-guide subject edges support preview and inspection.

The baseline missing-only seed remains a source-fixture operation. It does not
apply this conversion overlay. No live keyword assignments were changed while
preparing the plan.

# Editor guide staging bundle

This directory prepares the editor guide for a later OAuth-authorized import
into zotonic.com. Nothing in this bundle has been imported into production.

Start at [Zotonic for editors](index.md), or open the
[rendered preview](prepared/editor_guide.html).

## Contents

- 105 task and concept pages, intended as resources in the `userguide` category.
- 13 topic collections, inside one landing collection (`editor_guide`).
- 119 ordered `haspart` connections. “Why can’t I edit this page?” is one
  canonical page included in both People and access and Troubleshooting.
- Screenshots from the local test site's standard admin. No documentation
  dashboard or documentation-import controls appear in the images.
- `manifest.json`: stable names, properties, source files, media, and ordering.
- `prepare.py`: offline validation and rendering with the actual site extension.
- `prepared/`: disposable HTML previews and rendered resource bodies.

The structure follows the detailed page lists in the
[shared proposal](https://chatgpt.com/share/6ac53fef-99f0-83ed-805a-fa4421190569).
The additional Understanding Zotonic collection includes the proposal's
language and menu/collection concepts. Editorial notes and redirects are
included, with their scope explained. Optional functionality is identified in
collection metadata and introduced in plain language in the text.

## Editing and validation

Edit the Markdown files directly. Front matter uses YAML with JSON values,
one key per line. Keep its values synchronized with `manifest.json` if you
change metadata. Filenames, resource names, and category names are deliberately
independent of production numeric IDs.

From the workspace root:

```sh
python3 apps_user/zotonicwww2/priv/documentation/editor/prepare.py
python3 apps_user/zotonicwww2/priv/documentation/editor/prepare.py --render
```

Validation checks names, front matter, local links, image descriptions,
collection reachability, cycles, and order. Rendering needs the existing local
Erlang build, including `zotonicwww2_doc_link` and `markdownz`; it does not need
a running Zotonic server or an OAuth key. It uses a fresh local Erlang VM and
does not modify site data.

Markdown is converted with `zotonicwww2_doc_link:to_html/1`, including the site's
inline code reference extension, for example `model#rsc` and
`module#mod_translation`. The concept pages use these links sparingly. Ordinary
task instructions do not require knowledge of module names.

The renderer removes the first Markdown heading because Zotonic displays the
resource title separately. It rewrites page links to `/id/<stable-name>`.
Images in `prepared/resources.json` deliberately use `asset://<stable-name>`
until upload supplies real site media references. **That file is a staging
format, not a directly executable import payload.** The preview HTML replaces
those placeholders with local image paths.

## Later production import

Use normal authenticated Zotonic models with the supplied OAuth token. Never
store the token in this directory, a command history, or an import report.
Before implementing the HTTP import, inspect the deployed models and their
version; do not assume every local API is present on production.

1. Validate and render the final reviewed Markdown again.
2. Resolve `userguide`, `collection`, `image`, `haspart`, the intended content group,
   and all `editor_*` names on the destination. Check any existing matches
   before adopting them. These pages must remain separate from the automatic
   source-documentation importer and its deprecation tracking.
3. Upload screenshots as media resources, keeping the stable media names from
   the manifest. Record returned IDs, filenames/URLs, and content hashes in an
   import report so a retry reuses completed uploads.
4. Replace each `asset://` reference with the uploaded image's supported
   Zotonic media embed or URL. Preserve alternative text and captions. Do not
   copy local `.test` URLs, paths, or local resource IDs into production text.
5. Upsert the landing collection, topic collections, and text pages by their
   unique names. Map English strings to the deployed API's supported translated
   properties. Set the intended content group explicitly. New pages, collections and screenshots are
   published on import for review. Preserve publication state on existing
   resources and do not replace unrelated properties.
6. Create the manifest's `haspart` edges and explicitly apply their sequence.
   An edge insert alone is not sufficient to guarantee ordering on a retry.
   Only reconcile connections managed by this bundle. Do not delete unrelated
   connections on an existing page or collection.
7. Read back titles, bodies, languages, categories, image references, and the
   ordered edges. Check that all `asset://` references and local `.md` links
   have been resolved and that reference links open the intended pages.
8. Review the published pages and screenshots and connect the landing
   collection to the site's navigation.
   Decide how the existing User Guide should point to the new guide; do not
   overwrite an existing guide or claim its URL automatically.

The local source confirms resource reads and writes through `m_rsc` and
connection inserts through `m_edge`. Ordering and media-upload behavior must
be checked against the destination before import. No production importer or
OAuth credential is included or claimed to have been tested yet.

## Verification and local examples

Drafted against the current checkout and English test admin on 2026-10-06.
See [verification notes](VERIFICATION.md) for the UI checks, screenshots,
source references, and remaining production checks.

Two unpublished sample resources were created on `zotonicwww2.test`:

- `14584`: **Editor guide example: Community garden** (Text).
- `14585`: **Editor guide example: Garden menu** (Page Menu), containing 14584.

The sample menu is separate from the site's main menu. No existing content or
module configuration was changed. Required admin features were already enabled.

One test email was sent to the sample address `editor-guide@example.org` using
the user-confirmed catch-all configuration. Mailing run `1` reached **Finished**
with **Sent: 1**. This confirms mail-server acceptance, not inbox inspection.
No full-list mailing was sent. These local examples are not part of the
production import and can be retained for recapturing screenshots.

## Audience review and connected navigation

The [2026-10-07 review](../review/README.md) records page coverage, changes,
and remaining publication work. Collection contents come from `haspart`;
standalone related-task lists now use ordered `relation` edges. Curated reference
links use **`hasreference`**, distinct from `refers` managed by `mod_admin` and the
site's legacy `references` predicate.

Render both bundles to follow cross-guide links in the preview. `prepared/edges.json`
contains the curated graph plus generated keyword `subject` connections, not an executable API request. Importers
must apply **all three predicates**, resolve external targets and aliases, and
follow the [connection contract](../CONNECTIONS.md). Body HTML deliberately omits
connection lists; importing only bodies would lose their navigation.

## Surveys and forms

The [Surveys and forms collection](10-surveys-and-forms/index.md) adds 12 tasks
and three standard-admin screenshots. See the [verification record](../review/surveys-verification.md)
for the local form test, source checks, and remaining checks before publication.

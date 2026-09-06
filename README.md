# zotonicwww2

The second incarnation of the Zotonic web site - also used as an example site.

## Documentation import

The site maintains a disposable checkout of `zotonic/zotonic` below the
operating system's `TMPDIR`. Erlang module documentation, Markdown reference
pages, release notes, and generated EDoc are synchronized from that checkout.
The checkout is cloned again automatically if the operating system removes it.

The temporary location is intentional. Native dependencies built with GNU Make
can misinterpret whitespace in absolute target paths. On macOS, Zotonic's
default data directory contains `Application Support`, whereas the per-user
`TMPDIR` is normally space-free. If `TMPDIR` itself contains whitespace, the
checkout falls back to `/tmp`. Published EDoc remains in the site's durable
files directory; it is copied to a sibling staging directory before an atomic
replacement, so the temporary and data directories may be on different file
systems.

Administrators can inspect and control this process from the site dashboard.
The panel shows the checked-out, fetched, and last imported commits together
with the active stage, timestamps, result counts, and the last error. Every
successful import also reports keyword coverage per documentation category:
the number of pages with at least one subject keyword, the pages still missing
keywords, and the total number of assigned keywords.

The GitHub push webhook endpoint is `/github/webhook`. Configure the webhook
for JSON push events, set its secret to the value of `site.rebuild_secret`, and
set `site.rebuild_enabled` to `true`. Only pushes for
`zotonic/zotonic`'s `master` branch are accepted.

## Live migration

Schema version 16 installs two idempotent tracking tables, two content groups,
and the public faceted-search index:

- `Imported documentation`
- `Deprecated imported documentation`

The search index combines title, summary, and `subject` keywords in a
PostgreSQL trigram-indexed facet. The schema migration checks the facet table
and queues a full repivot, so it is safe to deploy before the content import.
While that repivot is running, public searches automatically fall back to the
regular full-text index.

Release-note imports extract the date following “released on” from the source
Markdown. The date is stored as `org_pubdate` with the resource and import
context set to UTC. Re-running **Import compiled docs** safely backfills these
dates on existing release-note resources; no schema migration is needed.

Installing the schema does not adopt, unpublish, or otherwise migrate existing
documentation. This keeps deployment separate from the content migration.

Use this sequence on the live site:

1. Back up the database and the site's files directory.
2. Deploy the code and reinstall or restart the `zotonicwww2` site module so
   schema version 16 is installed and its background repivot has completed.
3. In the admin dashboard, run **Fetch and rebuild**.
4. Verify the imported commit, counts, reference pages, and EDoc before making
   any legacy changes.
5. Review the number of legacy candidates shown in the dashboard.
6. Run **Migrate legacy imports**. This explicit, repeatable step adopts only
   recognized source-documentation names which were absent from the successful
   manifest, moves them to the deprecated content group, and unpublishes them.

The old RST dispatch mapper remains enabled. It must only be removed after old
page paths have been imported into `rsc_page_path_log` and a production URL
crawl confirms the redirects.

## Frontend compatibility

The public and admin templates continue to use Bootstrap 3 classes. Keep this
structure until the planned Bootstrap 5 migration using the compatibility CSS
from the `bs3-to-bs5` branch.

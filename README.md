# zotonicwww2

The second incarnation of the Zotonic web site - also used as an example site.

## Documentation import

The site maintains a disposable checkout of `zotonic/zotonic` below the
operating system's `TMPDIR`. Erlang module documentation, Markdown reference
pages, release notes, and generated EDoc are synchronized from that checkout.
The checkout is cloned again automatically if the operating system removes it.

The documentation build and import require Erlang/OTP 28 or newer. OTP 27
does not retain binary documentation literals such as
`-moduledoc(<<"...">>)` in the beam file's EEP-48 `Docs` chunk; the resulting
`module_doc` value is `none`. Running an import from an OTP 27 build can
therefore replace documented module pages, including `mod_survey` and
`mod_export`, with an empty body. Upgrade the production host to OTP 28 before
running **Fetch and rebuild** or **Import compiled docs**.

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

### Documentation references

An inline Markdown code span in the form `kind#name` links to the corresponding
imported reference page. The supported kinds and their public names are:

| Kind | Example |
| --- | --- |
| Template tag | `tag#print` |
| Template filter | `filter#escape` |
| Scomp | `scomp#wire` |
| Wire action | `action#update` |
| Template validator | `validator#presence` |
| Model | `model#rsc` |
| Controller | `controller#controller_page` |
| Zotonic module | `module#mod_base` |
| Notification | `notification#media_upload` |
| Dispatch file | `dispatch#mod_base/dispatch` |

References are resolved only in inline code. Fenced and indented code blocks,
existing links, unknown kinds, and Erlang references such as
`z_template:render/3` remain unchanged.

## Live migration

Schema version 16 installs two idempotent tracking tables, two content groups,
and the public faceted-search index:

- `Imported documentation`
- `Deprecated imported documentation`

The search index has a dedicated title facet and a combined facet containing
title, summary, category, and `subject` keywords. The public site query uses
PostgreSQL whole-string trigram similarity for titles and word similarity for
the combined text, with title matches ranked first. It runs through Zotonic's
search pipeline for ACL filtering, and the public search model always uses an
anonymous context. The schema migration checks the facet table and queues a
full repivot, so it is safe to deploy before the content import. While that
repivot is running, public searches automatically fall back to the regular
full-text index.

### Site-specific trigram operators

The two trigram branches intentionally use different PostgreSQL `pg_trgm`
operators:

| Facet | Match | Ranking | Default threshold |
| --- | --- | --- | --- |
| `ft_title` | `$1 OPERATOR(public.%) ft_title` | `similarity($1, ft_title)` | `pg_trgm.similarity_threshold`, normally `0.3` |
| `ft_important` | `$1 OPERATOR(public.<%) ft_important` | `word_similarity($1, ft_important)` | `pg_trgm.word_similarity_threshold`, normally `0.6` |

`%` compares the complete query with the complete title. `<%` searches for the
query as the best matching word extent inside the longer combined text. The
title match is intentionally more tolerant, which catches misspellings such as
`ifeqaul` without lowering the threshold for every page containing a related
word. Because the operators and thresholds differ, neither result set is a
strict superset of the other; the site query combines them with `UNION` and
ranks title matches first.

This implementation is local to `zotonicwww2` for now. It can move into
`mod_search` when facet query terms gain an explicit choice between
whole-value trigram matching (`%`/`similarity`) and word-extent matching
(`<%`/`word_similarity`). Keep using parameterized query terms and Zotonic's
normal search pipeline when making that change, so that ACL SQL continues to
be injected centrally.

The site also overrides `pivot/_related_ids.tpl`. Only outgoing `subject`
keyword ids are stored as `zpo...` tokens in `rsc.pivot_rtsv`; other predicates,
the content group, and categories are excluded. This makes both sides of the
`match_objects` query use the controlled subject vocabulary. Schema upgrade 22
queues all existing resources for repivoting so the stored vectors are updated
after deployment.

Release-note Markdown declares an ISO date in the YAML front-matter
`release_date` property. These values were initially derived from the release
text, with the corresponding Git tag date as a fallback. The importer treats
the explicit metadata as authoritative and stores it as the resource's
`publication_start` in UTC. Re-running **Import compiled docs** safely
backfills these dates on existing release-note resources; no schema migration
is needed.

Installing the schema does not adopt, unpublish, or otherwise migrate existing
documentation. This keeps deployment separate from the content migration.

Use this sequence on the live site:

1. Back up the database and the site's files directory.
2. Deploy the code and reinstall or restart the `zotonicwww2` site module so
   its current schema is installed and the background repivot has completed.
3. In the admin dashboard, run **Fetch and rebuild**.
4. Verify the imported commit, counts, reference pages, and EDoc before making
   any legacy changes.
5. Review the number of legacy candidates shown in the dashboard.
6. Run **Migrate legacy imports**. This explicit, repeatable step adopts only
   recognized source-documentation names which were absent from the successful
   manifest, moves them to the deprecated content group, and unpublishes them.
7. From an Erlang shell with the site context, run
   `zotonicwww2_convert:plan/1` and review the affected resources, edges, and
   page paths.
8. Run `zotonicwww2_convert:run/1` with the same context to install the legacy
   page paths and finish removing the old hierarchy.
9. Crawl the known production documentation URLs and verify their redirects.

The old RST dispatch mapper has already been removed. Legacy documentation URLs
can therefore be unavailable between deploying the code and completing step 8.

## Frontend compatibility

The public and admin templates continue to use Bootstrap 3 classes. Keep this
structure until the planned Bootstrap 5 migration using the compatibility CSS
from the `bs3-to-bs5` branch.


## External module documentation

Administrators can add and edit repositories using dialogs under **Content → External modules**
(`/admin/external-modules`), also linked from the documentation dashboard. Store
its public HTTPS Git URL, optional information URL, optional branch (empty means
the remote default), and Hex package. The registry records the adding user,
fetch/import timestamps, processing stage, errors, and imported/skipped files.
The repository table shows processing status, timestamps, and page counts. Open
**Import report** for the imported pages and errors for skipped pages.

**Deprecate** moves all documentation belonging to the repository into the
deprecated documentation content group, unpublishes it, and pauses updates.
An in-flight import cannot republish deprecated pages. Re-enable the repository
in **Edit** and save to import its documentation again.

Disable daily checks to pause a repository; existing pages remain available.
Saving or **Fetch and import now** queues a refresh. Disabled repositories are
not processed. No package is downloaded from Hex; it is shown as a package link.

The daily tick checks each enabled repository with `git ls-remote`. An unchanged
successfully imported commit is skipped. Changed repositories are shallow-cloned
into a disposable directory under the site's files directory. Git hooks,
credential prompts, redirects, non-HTTPS protocols, submodules, and build commands
are disabled. Each subprocess has a two-minute timeout and bounded output.
Git and escript must be available on the server; Erlang/OTP 28 is required.

A separate, disposable Erlang VM scans source with `erl_scan` and parses module
and moduledoc attributes with `erl_parse`. It never compiles repository code,
loads modules, expands macros, or evaluates include files. This keeps arbitrary
source atoms out of the site's VM. It accepts literal strings/binaries, OTP 27+
multiline strings, and `{file, "relative/path.md"}` docs confined to the checkout.
The scanner excludes `src/support` and `test` subtrees, including those in nested
applications. Internal helpers and tests do not appear in import reports or count
toward scan limits.
Symlinks are never followed. The limits are 2,000 Erlang/dispatch files, 2 MiB per
source or referenced documentation file, and 16 MiB of parsed data per repository.
Conditional branches are not evaluated; ambiguous or macro-based moduledoc
attributes are reported as skipped.

Files without nonempty moduledoc (including `false`), parse errors, duplicate
module names, and unknown subject-keyword slugs appear in **Pages not imported**.
Keywords use the same controlled subject vocabulary as core documentation.
A successful scan reconciles all pages for that repository in one transaction:
new pages are inserted, existing pages updated, and absent/skipped pages hidden.
An empty successful scan also hides all its former pages. A fetch or whole-scan
failure preserves existing pages. File-level errors do not prevent other files
from importing. The commit is recorded only after reconciliation succeeds.

Resource names start with `doc_external_<repository id>_`. Names are limited to
Zotonic's 80-character database limit; long or noncanonical Erlang names use a
readable prefix and a 128-bit SHA-256 suffix. Tracking uses a separate source key
per repository, so external imports cannot reconcile core or other repositories'
pages. Public pages show an external-module notice, distinct aside color, Git
and information URLs, and the optional Hex link. Module-component connections
use the documented module in the same source tree where possible.

Verification:

- `escript test/external_parser_test.escript` from this site directory tests source
  parsing, missing/hidden docs, macros, file docs, and symlink/path restrictions.
- EUnit tests in `zotonicwww2_external_import` cover URL/branch validation and
  bounded, collision-resistant resource names.
- Compile `test/zotonicwww2_external_integration.erl` and call `run(Context)` on a
  development site to test insertion, updates, hiding, restoration, repository
  isolation, HTML sanitization, admin-only reads, and template rendering. The
  integration test rolls back all its database fixtures.


### Module configuration tables

Both core and external module imports store `-mod_config` declarations in the
module page's `doc_module_config` property. The module page renders a table of
module, key, type, declared default, and description from this stored metadata.
An omitted module defaults to the declaring Erlang module; legacy `name` keys
are also supported. Defaults are displayed as Erlang terms, distinguishing
`false`, `undefined`, empty strings/binaries, and structured values. Live site
configuration is never read for this table. Reimport existing documentation to
populate the metadata; removing the attribute clears the stored table.

Core imports read the BEAM attributes without loading the module. External
imports parse literal `-mod_config` attributes in the isolated source-parser VM,
using the same normalization code. Unparseable attributes are reported with
other file-level parsing errors.


### External dispatch rules

Each application's `priv/dispatch/*` files are parsed as literal Erlang terms
inside the isolated VM. No code is evaluated or compiled. Hidden files, editor
backups, and symlinks are ignored. Multiple lists of rules per file are combined
in source order. Invalid syntax and malformed rules appear in the skipped report.

Each file produces one dispatch documentation page with rule names, paths,
controllers, and options. Controller links target documented controllers in the
same repository first, then existing publicly visible core controller documentation.
Unknown or unpublished controllers remain plain text. Pages retain the external-module notice and repository links,
and connect to the documented module in the same application. Missing or
ambiguous documented modules cause the dispatch file to be reported as skipped.
No synthetic module page is created when its source lacks moduledoc.

Names use `doc_external_dispatch_<repository id>_<path hash>` and remain within
80 characters. The full relative path distinguishes identically named files in
umbrella applications. Dispatch pages participate in the same transactional
updates and removal/hiding as other imported pages. Use **Fetch and import now**
to add dispatch pages from repositories whose Git commit has not changed.

Run `escript test/external_dispatch_parser_test.escript` from the site directory
for literal-term parsing, route ordering, dynamic paths, multiple applications,
malformed input, and file exclusion checks.


### External module observers

The isolated source parser reads literal `-export` attributes and recognizes
`observe_<notification>/2,3` and `pid_observe_<notification>/3,4`, matching
Zotonic's observer registration rules. Multiple callbacks for the same
notification produce one item; unexported functions and other arities are
ignored. Parsing still requires moduledoc and never compiles the module.

Imports store the complete list in `doc_module_observers` on the module page
and replace its `observes` connections to existing notification resources.
The Observes section links to visible notification documentation and lists
custom or undocumented notifications as plain names. The navigation count
includes both. Reimporting after removing exports clears the old names and
connections. Use **Fetch and import now** for an unchanged repository revision.

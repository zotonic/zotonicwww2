# Verification

Prepared on 2026-10-06 against the workspace source revision recorded in `manifest.json`.

## Completed checks

- 142 text pages and 13 collection resources (12 topics plus the root).
- 154 ordered `haspart` edges; all resources reachable from the root without cycles.
- Unique resource names and paths; matching Markdown front matter and manifest metadata.
- All relative source links resolve; every page has a title and substantive body.
- All 155 resources rendered using the built `zotonicwww2_doc_link` Markdown extension in a separate local Erlang VM.
- 586 local preview links resolve.
- The command reference covers all 39 `zotonic_cmd_*.erl` files in this checkout. Syntax and significant effects were checked against implementations, including differences from short help text.
- Source paths in the manifest exist. The 27 distinct reference identifiers correspond to local tags, scomps, models, filters, or modules. Their destination pages were not checked on the live production site.
- The complete `mod_garden` and `m_garden` examples compile with `erlc` and the built core behavior available. Compilation outputs were confined to a temporary directory.

## Scope and limits

The guide is an importable documentation draft. The preparation tool does not contact a running site. No developer-guide resources have been created on zotonic.com, and no OAuth credentials are present in the bundle.

Operational commands are documented from source, not executed as an acceptance test. In particular, site creation, restart, stop, restore, schema changes, and test runners were not run for this documentation work. The guide records their effects where relevant. Template and browser examples still need normal integration checks in the reader's application; compiling two Erlang examples does not verify every workflow.

There are no screenshots in this developer bundle. The existing editor bundle is separate. Local previews check the Markdown rendering and link preparation, not the final production site layout.

## Details checked carefully

- `connectdb` tests the global database connection and prints connection options, including the password; it does not open `psql`.
- `logtail` prints the last 500 lines and exits.
- `load` runs the changed-BEAM loader without a module selector.
- `rpc` passes trailing arguments as strings.
- `config` defaults to Zotonic configuration and reads file-based configuration.
- `setconfig` is temporary and only specially converts `true`, `false`, and `undefined`.
- `backup download` downloads **and restores** the newest backup.
- `runtests` discovers core test files, not arbitrary user application tests.
- `sitetest` stops the site and drops/reuses the `z_sitetest` schema before restoring normal site configuration.
- `wait` checks a node ping, not site readiness; its timeout path does not explicitly return a failure exit status.

To repeat structural validation and rendering, use the commands in `README.md`. Recheck source-sensitive guidance when updating Zotonic versions.

## Additional screenshots — 2026-10-07

See [capture record](../review/screenshots-2026-10-07.md) for the additional real-admin screenshots, their article placements, and verification limits.

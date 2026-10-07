# Audience review — 7 October 2026

Reviewed the two draft guides: 235 task/concept/reference pages and 26 collection
pages. [pages.csv](pages.csv) records the disposition of every resource;
[content-changes.json](content-changes.json) describes the 35 expanded task pages.
This is an editorial and targeted technical review, not a claim that every task
has been repeated in a browser or on a clean installation.

For each page, the questions were: who is doing this, what must they already
have, can they carry out the instructions, what result should they expect, what
happens if it fails, and where should they go for detail? Short concept pages
need an understandable distinction and a next task, not artificial numbered steps.

The subsequent [Surveys and forms addition](surveys-verification.md) adds 12 tasks
and one collection. The current inventory includes these 13 resources as well.

## Editor guide

The main learning path works for someone who maintains content without knowing
Erlang or the data model. Keep technical reference links as optional further
reading. The guide consistently distinguishes saving, publication, access,
navigation, and search indexing; those distinctions should survive the merge.

| Collection | Review and changes |
| --- | --- |
| Getting started | Added account recovery, admin-access check, sign-out, and a first-page completion/cleanup check. Still requires a manager-supplied account and admin address. |
| Creating and editing | Added failed-save recovery and concurrent-edit handling. Existing duplication, revision, deletion, and note tasks describe consequences adequately. |
| Publishing | Added how to cancel scheduled publication or publish now. Preserve the distinction between scheduling availability and scheduling a new text version. |
| Media | Added checks for the actual downloaded document, selectable text, charts, linked images, and alternative-text verification. Exact alt-text controls remain site-specific. |
| Organizing content | Added removing collection membership, preventing collection loops, correcting a connection, and why incoming links need no reverse edge. |
| Menus and navigation | Steps, persistence checks, nested-item consequences and public checks are sufficient for the standard menu editor. The website must still identify which menu it uses. |
| Languages | Added the immediate-publication risk of copying text into a published language and the effect of changing shared fields. Existing fallback and removal explanations are useful. |
| People and access | Marked account creation as administration and added acceptance checks with the new account. Ordinary editors should retain the permission/ownership explanations. |
| Mailing lists | Added small-batch import checks, replacement/export limitations, and cancellation verification. Keep list eligibility, previous deliveries, and server acceptance distinct. |
| SEO and sharing | Scope is sound: page titles/descriptions, no-index requests, previews, URL changes and redirects. Do not present no-index as privacy. |
| Troubleshooting | Checks lead from symptoms to a specific page, setting or manager request. Related-task connections now lead to the detailed procedure. |
| Understanding Zotonic | Concepts are short and use editorial examples. Added focused bridges to developer edge/media tasks without duplicating their technical explanations. |

The standard guide cannot describe every custom category, block, menu, image
layout or access policy. Sites should supply a short local supplement identifying
these choices, who reviews publication, and whom to contact. Do not fill that gap
with screenshots of the zotonicwww2 documentation dashboard.

## Developer guide

The intended reader can edit files and run commands, but is new to Zotonic.
Several original pages were outlines that required the reader to discover the
callback or file location independently. The additions provide a first working
example and an expected result at the main extension points.

| Collection | Review and changes |
| --- | --- |
| Getting started | Added prerequisite command checks, database assumptions, hostname resolution, dispatch file location and the expected trace. A clean-install exercise for the chosen release remains required. |
| Applications | Directory, package/module identity, priority, lifecycle and storage explanations are sufficient orientation. Related edges lead to implementation tasks. |
| Command-line workflows | Scope and effects are explicit. Keep the distinction between `connectdb` and psql, asynchronous build requests and success, and shell detach versus stopping the node. |
| Templates | Added standard base hooks/script flush, category filenames, overrules/inherit, cache dependencies and a first stylesheet. Existing reference spans now also produce `hasreference` edges. |
| Content and data | Added resource update/read-back, edge insertion/replacement semantics, a bounded search, and a complete installation datamodel. Full import reconciliation is documented separately in the connection contract. |
| Reusable functionality | Added a complete controller, observer, filter, queue API/return contract, and schema progression. Domain-specific background work still belongs to the application; queue insertion alone is not completion. |
| Browser and APIs | Added a model call over MQTT, its HTTP endpoint, a matching wire/postback, and a validated form/submit handler. Reading order now introduces model/Cotonic access before wires. |
| Development tools | Tool choice, activation, trace scope and stopping/cleanup are described. The source-checked settings are useful without enabling the unauthenticated development API. |
| Testing | Added a runnable EUnit example for a user application, outside the core-only `runtests` discovery. Retained permission, rendered-template and upgrade test guidance. |
| Configuration/deployment | Useful release preparation and recovery checklists, but not a complete operator installation manual. Move operator procedures into the administration guide; keep developer responsibility and links here. |
| Troubleshooting | Retained the focused diagnostic sequence rather than adding generic restarts/cache flushes. Each symptom links to the relevant tool or procedure. |
| Command reference | All 39 pages reviewed for syntax, prerequisite, scope and effect. These are lookup pages, not a sequence to execute. Keep the command implementation's limitations visible. |

The new short tasks complement deeper reference and recipe pages. They do not
justify deleting the existing material on notifier modes, full search/query
semantics, dispatch, identities, pivots, testing sites, email, or deployment.
The integration map remains a merge proposal. Existing broken/obsolete recipe
examples listed in `doc/cookbook-issues.md` must be repaired before recommending
them as complete worked solutions.

## Administration guide: still missing

There is a three-guide proposal, but no separate administration draft tree yet.
Do not label it complete or publish an empty entrance. Its initial coverage needs:

| Task | Available material | What is still needed |
| --- | --- | --- |
| Grant, change and revoke access | Editor account/permission pages | Exact group/rule procedure and tests as the affected account; account offboarding |
| Enable modules and change site settings | Developer activation/configuration pages | Admin procedure, dependencies, impact, and recovery from a failed activation |
| Install and expose a site | Local setup plus baseline deployment pages | One tested Zotonic release/OS/database/service/HTTPS path using the advised Erlang/OTP 28 |
| Configure outbound email | Editor mailing tasks and baseline email guide | SMTP/catch-all configuration and a complete delivery-failure diagnosis |
| Keep media durable | Developer storage overview | Concrete storage setup, retrieval and restart checks |
| Restore a site | Backup CLI and recovery checklist | Tested restore of database, media, configuration and security material together |
| Update and monitor | Deployment/testing/troubleshooting pages | Operator runbook, readiness checks, rollback limits, and recurring-failure handling |

These are concrete publication gaps, not reasons to block review of the existing
two guides. Preserve their source material and stable names during conversion.

## Datamodel changes

- 260 ordered `haspart` edges define contents; duplicate Markdown contents lists
  have been removed from all 26 collection pages.
- 421 ordered `relation` edges supply related tasks, including focused cross-guide
  links and the model prerequisite for browser messaging.
- 47 `hasreference` edges target 32 existing reference resources. All 32 names
  resolved on the local test site; the registry records their current titles.
- Explanatory links remain in paragraphs when needed for a step. Standalone
  suggestions are editable connections, not duplicated body text.
- The new `hasreference` predicate and generic site panel are included in the
  application. `refers` stays under `mod_admin` ownership. Legacy `references`
  data is unchanged.

See [CONNECTIONS.md](../CONNECTIONS.md) for import ordering, aliases, preservation
of editorial additions, retries, and rendering. Importers must apply the graph,
not only rendered bodies.

## Verification and limits

- Five graph tests pass, including missing targets, duplicate targets/order, and
  collection cycles; reciprocal related tasks remain valid.
- Both manifests validate and render all 261 resources using
  `zotonicwww2_doc_link`. Nine existing screenshots remain registered and linked.
- Seven Erlang example modules compile, including the combined postback/submit
  clauses, controller, observer, datamodel, model, filter and filter test.
  The filter EUnit example passes. The changed site module compiles.
- All 17 fenced template examples compile with the current Zotonic template
  runtime. Compilation alone does not prove every example's complete HTTP flow.
- The connection partial renders on the running local test site. A transactional
  fixture checks that anonymous readers see published targets and cannot see
  unpublished targets, while an administrator can. `hasreference` renders as
  further reading. Fixture page/edge writes were rolled back.
- The site file watcher applied schema 26 locally and created `hasreference`.
  Neither draft guide was imported or published by this review.
- No production writes, new screenshot session, clean OS/database installation,
  destructive restore test, or full browser interaction test was performed.
  Earlier screenshot/UI verification remains in each guide's `VERIFICATION.md`.

Erlang/OTP 28 is the advised version, as decided on 7 October 2026. The local
setup guide includes a command that checks the OTP release rather than the
separately numbered emulator version.

Before publication, run the reviewed learning path on OTP 28 with the selected Zotonic release,
complete the operator procedures above, and perform the combined local import
with alias/ownership checks and an unchanged second run. The offline source and
preview checks are preparation for that test, not a replacement for it.

## Installation follow-up

The [installation review](installation-review.md) rewrites the first-run path and
adds five supporting setup tasks. The inventory now contains 252 text pages and
27 collections across both guides; the original review counts above are historical.

## Administration guide

Added 30 operator tasks and eight collections; five existing tasks are shared
through collection membership. See the [proposed tree](../administration/TREE.md)
and [verification limits](../administration/VERIFICATION.md). The current inventory
contains 282 text resources and 35 collections across all three guides.

## Guide category correction

All 282 task pages now use their guide category: 105 `userguide`, 147
`developerguide`, and 30 `adminguide`. The 35 collections remain `collection`.
Shared tasks retain the category of their source guide. Manifests, Markdown
front matter, integration source categories, and rendered payloads agree.
Schema 27 adds `adminguide` below `documentation`; the local site confirms
its inherited `text` type. The validator rejects generic or wrong-guide task
categories. All seven connection/category tests pass. Original baseline exports
and checksums remain unchanged.

## Media sandbox and mediarunner

Added the administration task on local sandbox enforcement and optional remote
processing, including setup and consumer management checked against the local
mediarunner checkout. The current inventory contains 318 resources: 283 task
pages and 35 collections. See administration/VERIFICATION.md for source details
and runtime verification limits.

## Controlled keywords

Assigned canonical taxonomy keywords to all 318 authored resources and the
94 existing baseline pages (including 45 cookbook entries). Information type,
audience, and focused subject assignments are validated. The additive import
plan resolves adoption aliases and preserves existing unrelated keywords.
See [keyword coverage and application rules](../KEYWORDS.md).

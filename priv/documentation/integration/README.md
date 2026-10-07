# Proposal: one documentation structure, three guides

Status: proposal for review, based on the public site and local drafts inspected on 6 October 2026. No site content, templates, or existing draft manifests have been changed.

Use **Editor guide**, **Developer guide**, and **Site administration** as the three entry points. Merge the existing guides with the drafts, preserving useful explanations, worked examples, and existing links. The three draft directories are source material for the merge, not additional guides to publish alongside the existing ones.

## What is there now

The [Start page](https://zotonic.com/start) presents two guide cards. Its resource is `page_start` (2695), a collection with no `haspart` children. The local Start template hardcodes the two cards by resource name. Adding collection connections alone will not make a third card appear.

The [User Guide](https://zotonic.com/user-guide), `doc_userguide_index` (2173), has four children: a CMS introduction, a data-model explanation, user management, and reporting issues. The introduction and user-management coverage are very short. The data-model page has a useful worked example that should survive the merge.

The [Developer Guide](https://zotonic.com/developer-guide), `doc_developerguide_index` (1635), has 30 direct children. It mixes learning, framework concepts, operating a site, and release information in one long list. Some existing pages are much more detailed than the new drafts: Modules covers lifecycle and dependencies; Notifications explains the different notifier operations; Resources includes blocks, identities, pivots, and deleted resources. The new short task pages should be entrances to that material, not replacements that discard it.

The live [Cookbook](https://zotonic.com/cookbook) is category resource 318. A public category search returned 45 entries. It contains useful complete examples, background lessons, small snippets, and placeholders. The older `/cookbook/1571/cookbooks` page appeared in cached search results but returned 404 from the live API; do not use it as the inventory authority.

The [Reference](https://zotonic.com/reference) and [command-line page](https://zotonic.com/reference/command-line) already provide a home for lookup material. The 39 draft command pages belong here. Release history already has its own [landing page](https://zotonic.com/docs/1643/release-notes).

The drafts now contain 105 editor pages and 147 developer pages, plus 27 collection resources in total (including Surveys and forms, added on 7 October 2026). The developer count includes 39 command pages. These are source counts, not the recommended final page count: several pages can be merged, and existing material fills gaps.

## Names and navigation

Keep `/start` and its title **Start with Zotonic**. Change the navigation label **Start** to **Guides**; keep the existing URL. On that page, use these three cards:

| Card | Description | Destination |
| --- | --- | --- |
| Editor guide | Create, organize, translate, and publish content. | Adopt the existing `/user-guide` resource; rename its title. |
| Developer guide | Build sites, templates, modules, and integrations. | Adopt the existing `/developer-guide` resource. |
| Site administration | Manage access, configure services, and keep sites running. | New collection; proposed path `/admin-guide`, subject to a collision check. |

The resulting primary navigation would be **Guides · Recipes · Reference · Releases**. Rename Cookbook to **Recipes**, keeping `/cookbook`. This makes the purpose clear without requiring a URL migration.

The third guide covers two kinds of administration: browser-based site administration and technical operations. Mark prerequisites on individual tasks: “Site administrator”, “Server access required”, or “Database access required”. Do not imply that every editor or site administrator has shell access.

A small footer or secondary navigation can link to Help and contributing. Keep the glossary shared, with reader-specific explanations linked from the guides.

## Proposed collection tree

All groupings below are collections. Individual instructions and explanations are text resources. Reference retains its existing component/category browsing, with an explicit command-line collection alongside it.

```text
Start with Zotonic / Guides
├── Editor guide
│   ├── Getting started
│   ├── Content basics
│   ├── Create and edit content
│   ├── Publish content
│   ├── Images and files
│   ├── Content and navigation
│   ├── Translate content
│   ├── Newsletters
│   ├── Surveys and forms
│   ├── Search engines and sharing
│   └── Troubleshooting
├── Developer guide
│   ├── Getting started
│   ├── Applications and modules
│   ├── Templates and page rendering
│   ├── Content and data
│   ├── Extending Zotonic
│   ├── Browser interaction and APIs
│   ├── Development tools
│   │   ├── Command line and Erlang shell
│   │   └── Inspect and debug a running site
│   ├── Testing and contributing
│   └── Troubleshooting
└── Site administration
    ├── People and permissions
    ├── Site settings and modules
    ├── Installation and deployment
    ├── Configuration and services
    ├── Backups and recovery
    ├── Updates and maintenance
    └── Monitoring and troubleshooting

Recipes
├── Customize the admin
├── Pages and browser interaction
├── Content and integrations
└── Operations recipes

Reference
├── Existing component references
├── Command-line reference (39 command pages)
├── Configuration reference
└── Installation requirements

Releases
├── Release notes
└── Version-specific upgrade notes
```

The developer subcollections keep the extensive tools material navigable. Do not show every page and every subgroup in the top-level menu; collection pages can list their own ordered children.

## Integrate the editor draft

Use the new editor text and screenshots as the main task coverage. Retain the current guide resource and URL, and map the draft root `editor_guide` to it.

| Draft collection | Proposed treatment |
| --- | --- |
| Getting started | Keep; merge its introduction into the existing short CMS introduction. |
| Understanding Zotonic | Rename Content basics and move near the beginning. Use the current data-model example as background, expressed in editor language. |
| Creating and editing content | Rename Create and edit content. |
| Publishing | Rename Publish content. |
| Media | Rename Images and files. Explain “media” inside the guide. |
| Organizing content + Menus and navigation | Combine under Content and navigation. Explain the difference between category, collection, connection, and menu before the tasks. |
| Languages and translations | Rename Translate content. |
| Mailing lists and newsletters | Rename Newsletters; keep recipient and test-send tasks. |
| Surveys and forms | Keep the form-building, testing, response handling, export, and quiz tasks together. Link module configuration to Site administration. |
| SEO and sharing | Rename Search engines and sharing. |
| People and access | Split by task as described below. |
| Common tasks and troubleshooting | Rename Troubleshooting; avoid making it a second miscellaneous task collection. |

Move **Creating a user**, **Collaboration groups**, and **Upload restrictions** to Site administration. Rewrite the latter two where necessary: the current drafts largely describe what an editor sees, so relocation alone does not provide a complete administrator procedure. Keep **What am I allowed to edit?**, **Why can’t I edit this page?**, and the editor explanation of **Content ownership** in the Editor guide, with links from administration. Move the introductory user-role explanation into Content basics.

Keep newsletters, publishing dates, translations, SEO fields, and editorial redirects with the editor. Move service configuration, enabling site languages, server redirects, and delivery failures to administration. These are related but different tasks; do not merge them merely because they mention the same feature.

Use the existing “Issues and features” page as Help / Report a problem, accessible from all three guides. Remove its unverified release-schedule promise during editorial review.

## Integrate the developer draft

Use the new collections to organize the existing technical handbook. Keep detailed existing content after checking its examples against the supported Zotonic version.

| Existing material | Proposed home and merge |
| --- | --- |
| Introduction, Getting Started, Docker | Getting started. Adopt the authored installation entry page and container walkthrough under the existing identities. Keep native, Nix, PostgreSQL, and build/start tasks separate. Use `baseline/revisions/installation.json`; move Cloud-Init to later deployment review. |
| Directory structure, Sites, Modules | Applications and modules. Merge the draft layout and lifecycle explanations while preserving `_checkouts`, dependencies, startup, and schema details. |
| Templates, Media, Icons, CSS classes | Templates and page rendering. Keep a clear syntax introduction and rendering examples; move raw CSS/icon lookup material to Reference where appropriate. |
| Resources, Search, data-model explanation | Content and data. Retain blocks, identities, search syntax, pivots, indexing, and deletion behavior missing from the concise draft. |
| Controllers, Dispatch rules, Notifications | Extending Zotonic. Keep complete routing and notification contracts, supplemented with the new task pages. |
| Browser/server interaction, Wires, Forms | Browser interaction and APIs. Explain the choice of interaction mechanism before the examples. |
| Shell, Logging, new CLI workflows and Development module tools | Development tools. Separate writing diagnostic code from operating logs and services. |
| Testing sites, Contributing | Testing and contributing. Keep the working site-test setup and examples; update documentation-contribution instructions for the current tools. |
| Deployment, configuration, Status site | Site administration, with links from developer setup and troubleshooting. |
| Release Notes, Upgrade notes | Releases. Administration links to the relevant upgrade instructions from its update workflow. |
| E-mail handling | Split: sending/receiving from application code in Extending Zotonic; relay, identity, delivery, and catch-all configuration in Site administration. |

The current browser/server guide explicitly leads with MQTT, while the new draft starts with wires. Resolve this during the merge: lead with models and Cotonic/MQTT for new browser code, and document wires/postbacks for the template and admin workflows that use them. Do not label all wires unsupported or remove useful existing examples.

Consolidate obvious repetition within the drafts:

- Combine connecting to the shell and inspecting data with the existing shell guide.
- Merge CLI dispatch inspection with the Development dispatch troubleshooting task.
- Combine interface translation and POT extraction into the existing technical translation guide; keep editor translation tasks separate.
- Merge compile/load and recompile/rescan explanations into one workflow, linking to exact command references.
- Combine backup workflow pages in Site administration.
- Give “find the selected template” one canonical task, linked from templates and debugging.

The complete model recipe based on an external API is useful beyond the new minimal model example. Keep both with distinct purposes: first model in the guide; full integration in Recipes. Conversely, the existing “Writing your own module” recipe is a placeholder: replace its content with the new complete module walkthrough and retain its identity.

## Build the third guide from existing material

| Collection | Material available now | Work still needed |
| --- | --- | --- |
| People and permissions | Existing user-management page; editor user, group, permission, and ownership material; password-recovery recipe. | Complete create/deactivate-user, role assignment, and access-review procedures, with current UI screenshots. |
| Site settings and modules | Status-site page, module activation material, language/configuration explanations. | Separate browser-admin tasks from node-level tasks; document enabling languages and configuring upload limits. |
| Installation and deployment | Existing deployment children; draft deployment workflow and environment separation. | Check service startup, supported installation choices, reverse-proxy and HTTPS examples against the chosen version. |
| Configuration and services | Existing global/site/port configuration, email handling; draft configuration and storage pages. | Task-based setup pages for SMTP, media storage, TLS, and hostnames; link exact setting definitions to Reference. |
| Backups and recovery | Draft backup and recovery pages, command reference, existing restoration recipe. | Verify a complete current restore procedure with database, media, configuration, and security files. |
| Updates and maintenance | Upgrade notes, draft release and upgrade checks. | A clear operator checklist linked to version-specific changes. |
| Monitoring and troubleshooting | Existing logging configuration and log integration recipes; site-startup, database, and media diagnosis. | Health checks, mail delivery diagnosis, useful alerts, and current recovery/escalation guidance. |

The [administration draft](../administration/README.md) now implements this tree with 31 new tasks, seven topic collections, and five shared task members. See its [full proposed tree](../administration/TREE.md) and [verification record](../administration/VERIFICATION.md). Deployment-specific examples and runtime checks remain publication work.

## Recipes, reference, and concepts

Keep Recipes as a separate format for an end-to-end example with a visible result. Basic recurring tasks belong in the relevant guide. One page can be linked from a guide and from Recipes without copying its body.

“Admin cookbook” material describes **developing the admin interface**, not administering a site. Rename that grouping **Customize the admin** and link it from the Developer guide. Split the former “Other” grouping by purpose. Put “Just enough…” background lessons under supplementary developer foundations, with clear version review for old Erlang/rebar material.

Move the draft command-reference collection under the existing command-line landing resource (1547). Keep one canonical page per command and cross-link it from developer and administration tasks. Retain the environment-variable link and add current version/source information. Do not maintain a second 39-command list inside the guide body.

Keep detailed settings tables in Configuration reference. Administrative pages should explain tasks and point to those definitions. Preserve source-generated reference pages and the Markdown extension's stable `doc_*` links.

Share definitions through a glossary where useful, but keep audience-specific explanation. An editor needs to understand how connections affect a page; a developer also needs predicates, edge APIs, ACL context, and ordering.

## Preserve identities and make the import repeatable

Prefer reusing an existing resource when it answers the same question. A navigation or title change does not require a new ID, unique name, or URL. Change guide roots to the collection category only after checking their current templates, category-dependent behavior, and importer ownership.

The proposed root aliases are:

| Draft identity | Existing or proposed destination |
| --- | --- |
| `editor_guide` | `doc_userguide_index`, existing ID 2173 |
| `developer_guide` | `doc_developerguide_index`, existing ID 1635 |
| `developer_collection_command_reference` | `doc_reference_cli_index`, existing ID 1547 |
| `admin_guide` | New `admin_guide` collection; resolve name and path conflicts before creation. |

For a multi-page split, retain the old resource as a useful overview with links to the new tasks. Redirect only when a page has a clear single successor and its useful content has been transferred. Check old anchors as well as page URLs. Do not redirect every old guide page to a landing page.

Existing guide pages have GitHub source links, while the current source-documentation importer can overwrite managed titles, bodies, categories, and edges. Public export does not prove whether each old page is still actively managed. Audit importer tracking before adopting pages; deliberately transfer ownership or change the authoritative source. Merely importing new bodies over them is not a durable merge.

When the proposal is accepted, generate a combined staging manifest from the three source trees and the reviewed existing pages. Resolve aliases in body links, edges, media, and root navigation before import. Preserve editor screenshots, alternative text, and media names. Do not change `editor_*` or `developer_*` stable names merely because a page moves to administration.

The Start template must be changed to show three guide resources, and its dedicated grid currently forces two columns. Prefer an ordered `haspart` list on `page_start`, rendered by the template, so the guide choices become editable content. Check collection breadcrumbs and mobile layout. This is proposed implementation work, not a change made by this review.

## Recommended sequence

1. Agree the three audiences and the collection names; use the attached mapping as the working inventory.
2. Check ownership of reused live resources and choose the supported Zotonic version. Use Erlang/OTP 28 as the recommended Erlang version and update reused installation instructions accordingly.
3. Merge the editor guide first; it fills the largest user-facing gap and already has screenshots.
4. Merge developer explanations and examples topic by topic. Replace placeholders, preserve detailed coverage, and add the new Development tools tasks.
5. Review the authored Site administration tasks against the chosen deployment, merge mapped material, and run the remaining operational checks.
6. Stage the shared reference and recipe links, aliases, ordered collection edges, and Start navigation changes.
7. Import new pages, collections and screenshots as published for review. Verify bodies/media/URLs/old links and navigation; preserve publication state on existing resources.

## Review files and limits

- [Live inventory](live-inventory.json): selected public metadata, headings, word counts, and ordered children for 110 resources fetched through the public export API. It includes adjacent reference and release roots; it is not a full crawl of the entire site.
- [Existing-content map](existing-content-map.csv): proposed treatment of every fetched resource. Recipe dispositions are structural recommendations, not a claim that every code sample was executed.
- [Draft-content map](draft-content-map.csv): proposed destination and merge target for every resource in both draft manifests.
- [Priority review queue](review-queue.md): known gaps and conflicts to resolve before publication.

All network requests for this review were anonymous reads. No imports, redirects, deletions, or source-template changes were performed. The two draft bundles remain intact and their individual manifests remain valid; the integration maps are proposals, not executable import payloads.

## Installation replacements

The [installation revision map](../baseline/revisions/installation.json) replaces
the old Getting Started, Docker, and Installation requirements bodies with the
reviewed draft texts. Requirements remains in Reference; the onboarding page links
to its new text. Keep build/start as its own task, not a second replacement for
Getting Started. The [review record](../review/installation-review.md) covers the
split, preserved identities, and verification limits.

## Administration draft added

The administration bundle adds 39 owned resources (31 tasks and eight collections)
and reuses five existing tasks by `haspart`. There are now 318 owned resources
across the three bundles. Shared membership does not increase this resource count.
The developer deployment and backup summaries point to the operator procedures;
merge or redirect those summaries during conversion rather than publishing
duplicate checklists.

## Keywords during import

Run the [combined keyword preparation](../KEYWORDS.md) after resolving adoption
mappings. Add its `subject` connections to existing articles and cookbooks as
well as new guide pages, preserving unmanaged existing keywords.

## Baseline text revisions

The [complete baseline review](../baseline/revisions/README.md) now covers all
captured cookbooks and other baseline text pages. Its prepared import plan is the
final body writer after the adoption steps above. It merges multiple adopted
sources before updating a destination, preserves source identity and existing
fragment links, and carries additive reference/keyword connections.
The XSS page (source ID 2325) uses canonical baseline name
`doc_cookbook_security_templates_xss`; `developer_template_values` is its alias.

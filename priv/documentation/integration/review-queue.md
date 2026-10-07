# Review queue before integration

This queue distinguishes a structural destination from a publish-ready page. Moving a page does not verify its commands, examples, or completeness.

## Resolve first

| Item | Evidence | Required decision or work |
| --- | --- | --- |
| Erlang version — decided | Recommend **Erlang/OTP 28** for setup, development, and deployment. Older live installation pages and build minima differ. | Update reused installation pages to recommend OTP 28 during conversion. Keep minimum-supported versions distinct from this recommendation; verify the complete setup on OTP 28. |
| Who owns existing pages | Public guide exports contain GitHub source URLs. Local `zotonicwww2_doc_import` can replace managed resource properties and edges. | Inspect tracking on the destination before taking over a resource. A missing public `doc_source_kind` field does not establish that no importer owns it. |
| Three guide cards | `page_start` has no children; `page.name.page_start.tpl` hardcodes two resources; `.start-guides__grid` sets two columns. | Add ordered guide relationships and update rendering/layout. Importing a third collection alone cannot update this UI. |
| Root identity and category | Existing guide roots are documentation subcategories of text. New draft roots are collections with different names. | Reuse existing IDs/names and URLs where possible; check category-dependent templates before converting to collections. Apply explicit aliases to draft links. |
| Browser interaction direction | [Current interaction guide](https://zotonic.com/id/1302) favors MQTT; the draft starts with wires. | Introduce the current model/Cotonic/MQTT approach first. Keep wires and postbacks where useful, with their scope explained. |

## Preserve useful material absent or abbreviated in the drafts

- Installation alternatives and a verified first-run database setup from [Getting Started](https://zotonic.com/id/1526) and [Docker](https://zotonic.com/id/1411).
- Template syntax, variable/model access, and inheritance examples from [Templates](https://zotonic.com/id/1352).
- Notifier modes and callback contracts from [Notifications](https://zotonic.com/id/1274).
- Dispatch matching and URL generation rules from [Dispatch rules](https://zotonic.com/id/1541).
- Module dependencies, startup, schema/lifecycle details from [Modules](https://zotonic.com/id/1353).
- Resource blocks, identities, pivots, indexing, and deletion behavior from [Resources](https://zotonic.com/id/1276).
- Complete site-test setup and discovery rules from [Testing sites](https://zotonic.com/id/1546).
- Email code, configuration, and delivery handling from [E-mail handling](https://zotonic.com/id/1552), split by reader task.
- Complete external-API, controller, queue, and admin-customization examples from Recipes. The concise draft pages do not replace these worked examples.

## Replace or repair

- [CMS introduction](https://zotonic.com/id/1819): replace the placeholder with the draft editor introduction.
- [Writing your own module](https://zotonic.com/id/1576): replace the placeholder with the new complete module example, keeping the identity.
- [Create a custom action](https://zotonic.com/id/1340): still needs a complete task; the draft has no equivalent full action implementation.
- [User management](https://zotonic.com/id/2045): adopt the checked current create-user task and extend the new administration collection.
- [Command-line reference](https://zotonic.com/id/1547): merge the source-checked 39-command reference. Check differences such as `rpc` argument handling and `logtail` line count. Retain useful environment-variable links.
- [Automatic startup](https://zotonic.com/id/1987), [Varnish](https://zotonic.com/id/1990), [nginx](https://zotonic.com/id/1790), and [port configuration](https://zotonic.com/id/1482): validate deployment examples for the supported setup before recommending them.
- [Database recovery recipe](https://zotonic.com/id/1599): verify a complete restore on an isolated test site. Do not carry its old command sequence into the new guide unchanged.
- [Contributing](https://zotonic.com/id/1642): update documentation authoring/build instructions to the current Markdown and source-reference workflows.
- Old “Just enough” Erlang/rebar lessons and [Icons](https://zotonic.com/id/1637): check version-dependent commands and asset tooling before featuring them.

## Fill administration gaps

The proposed third guide has source material but still needs current task pages for:

1. Assigning roles, removing access, and reviewing effective permissions.
2. Creating and managing collaboration groups; the editor draft alone is insufficient.
3. Enabling languages, setting fallbacks, and configuring upload restrictions.
4. Configuring outbound email and a test catch-all; diagnosing delivery failures.
5. Setting up persistent media storage and checking retrieval after restart.
6. Selecting a supported service-startup and HTTPS/proxy arrangement.
7. Restoring database, files, configuration, and security material together.
8. Applying an update with readiness checks and a tested recovery plan.
9. Monitoring site health and responding to recurring failures.

Where a task is browser-based, capture a current standard-admin screenshot. Continue excluding the zotonicwww2-specific documentation dashboard. Do not replace the existing editor screenshots with site-specific developer UI.

## Before import and publication

- Keep the old body and outgoing-edge inventory for each adopted resource so an editorial merge can be reviewed and reversed.
- Record final destination IDs only after resolving names, paths, and ownership on the destination.
- Generate a combined manifest from reviewed content. The CSV maps here are not an import payload.
- Rewrite all aliases after consolidation, including body links and collection edges.
- Preserve ordered `haspart` connections and only reconcile relationships owned by this documentation change.
- Keep one primary navigation home for a page. Additional contextual links or collection membership must not create cycles or misleading breadcrumbs.
- Upload/reuse the editor screenshot resources and resolve their staged asset links.
- Check old named URLs, numeric-ID URLs, meaningful page paths, and anchors. Use a specific successor for each redirect.
- Verify media, anonymous visibility, translated titles, reference links, breadcrumbs, mobile navigation, and search results on the staged content.
- Publish all new documentation pages, collections and screenshots on import so they can be reviewed. Record remaining content issues in this review queue; preserve publication state on existing resources.

## Follow-up from the 2026-10-07 audience review

See [the review](../review/README.md) for 35 expanded task pages, compiled examples,
and the remaining audience gaps. Browser/API collection order now introduces
Cotonic/model access before wires. New guides use connection-driven collection
contents and related reading. Preserve all `relation` and `hasreference` targets
when applying root aliases or merging pages. `refers` remains managed by
`mod_admin`. The administration tree and the release/deployment-specific work
listed above are still required; this review does not mark them complete.

# Administration guide staging bundle

Start at [Site administration](index.md), inspect the [proposed tree](TREE.md),
or open the [rendered preview](prepared/admin_guide.html).

This bundle adds 31 task pages, seven topic collections, and the `admin_guide`
landing collection. Its collections also include five existing editor/developer
tasks by `haspart`, without copying their bodies or changing their stable names.
Each new task states whether it requires browser administration, server access,
database access, or another service owner.

The seven collections follow the agreed integration proposal. The pages are
editable English Markdown, with new pages, collections and screenshots set to
be published on import for review. They do not
configure the local site or perform deployments, account changes, or restores.

## Validate and preview

From the workspace root:

```sh
python3 apps_user/zotonicwww2/priv/documentation/editor/prepare.py --render
python3 apps_user/zotonicwww2/priv/documentation/developer/prepare.py --render
python3 apps_user/zotonicwww2/priv/documentation/administration/prepare.py --render
python3 -m unittest discover -s apps_user/zotonicwww2/priv/documentation/review -p 'test_*.py'
```

Render all three bundles to follow shared-page links in the preview. The shared
renderer uses `zotonicwww2_doc_link`, including the site's Markdown reference
extension. `prepared/resources.json` and `prepared/edges.json` are intermediate
outputs, not directly executable API requests.

## Import and merge

Resolve `admin_guide` and a proposed `/admin-guide` path on the destination before
creating them. Use the integration maps to resolve existing identities and
overlapping content. Installation is shared with the developer walkthrough;
configuration definitions and command syntax remain linked reference material.

Follow the [connection contract](../CONNECTIONS.md) for `haspart`, `relation`, and
`hasreference`. Shared task membership does not transfer ownership of the task's
body or publication state. Keep collection trees local to each source bundle;
shared guide pages are leaves, avoiding cross-guide collection cycles.

The authored operating tasks should become the canonical operator procedures.
The short developer deployment and backup pages remain in their current bundle
until conversion; merge their useful material and redirect only when there is a
clear successor. Do not publish two competing operator checklists.

See [verification and publication work](VERIFICATION.md). Exact service units,
proxy configurations, DNS records, and provider credentials belong to the chosen
deployment's operating notes; this guide does not invent a universal deployment.

New task pages use `adminguide`, installed below `documentation` by site schema
27. Collections remain `collection`; reused pages keep their guide category.

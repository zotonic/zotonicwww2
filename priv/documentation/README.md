# Documentation sources and conversion fixtures

Permanent home for the documentation previously staged in `tmp-*` directories.
These files belong to the zotonicwww2 application and can be located at runtime:

```erlang
filename:join([code:priv_dir(zotonicwww2), "documentation"]).
```

| Directory | Purpose | Entry point |
| --- | --- | --- |
| `import/` | Portable plan and Erlang import instructions | [Run the importer](import/README.md) |
| `editor/` | New editor guide Markdown and screenshots | [Guide](editor/index.md), `editor/manifest.json` |
| `developer/` | New developer guide Markdown | [Guide](developer/index.md), `developer/manifest.json` |
| `administration/` | Site administration tasks and proposed tree | [Guide](administration/index.md), [Tree](administration/TREE.md) |
| `integration/` | Proposed three-guide structure and merge mappings | [Proposal](integration/README.md) |
| `baseline/` | Public exports, reviewed corrections and local import helpers | [Baseline instructions](baseline/README.md), `baseline/manifest.json` |

The editor, developer, and administration manifests describe the proposed additions. The baseline
manifest describes existing source-site resources. Keep those roles separate in
conversion routines: source IDs are not local IDs, and the integration CSV maps
are editorial proposals, not executable import requests.

Resource names are the primary matching keys. The baseline also records source
URIs and explicit local names for source resources without names. Preserve those
aliases when testing conversion or moving pages between collections.

The editor guide includes [Surveys and forms](editor/10-surveys-and-forms/index.md):
creating and testing forms, managing responses, exporting results, and quizzes.

## Recommended Erlang version

Use **Erlang/OTP 28** for the documentation’s setup, development, and deployment
examples. Apply this recommendation when converting existing installation pages.
The baseline exports and live inventory remain historical source snapshots.

## Validate and preview the new guides

From the Zotonic workspace root:

```sh
python3 apps_user/zotonicwww2/priv/documentation/editor/prepare.py --render
python3 apps_user/zotonicwww2/priv/documentation/developer/prepare.py --render
python3 apps_user/zotonicwww2/priv/documentation/administration/prepare.py --render
```

These scripts discover the workspace from their parent directories. They validate
the sources and render with the site's Markdown extension. Their `prepared/`
directories are generated previews and are ignored by Git. The portable import plan is generated from these sources by `build_import_plan.py`;
deploy that plan with the sources for the Erlang importer.

## Import on test and production sites

Use [the Erlang importer](import/README.md) to audit and import the complete bundle.
Build its portable plan with `build_import_plan.py`, deploy it with the application,
and run `zotonicwww2_documentation_import:audit/2` followed by `import/2` in the
destination server's shell. The site and environment must be specified explicitly.
The importer handles baseline adoption, new guides, final corrections, media,
keywords and connections in one run. No separate seed or correction pass is needed.

The older [baseline helpers](baseline/README.md) remain available for isolated
fixture testing. They do not implement the complete documentation import.

## Documentation review

See [the audience review](review/README.md) and its [page inventory](review/pages.csv).
The [connection contract](CONNECTIONS.md) describes `haspart`, `relation`, and
`hasreference`, including identity resolution and ownership during repeat imports.
`reference-targets.json` records existing reference resource names verified on the
local site. `prepare_common.py` implements validation/rendering for all three bundles.

## Guide categories

Task pages use `userguide`, `developerguide`, or `adminguide` for their source
guide. All three are children of `documentation`, itself a child of `text`.
Landing pages and topic collections remain `collection`. Shared collection
membership does not change a page’s category. Site schema 27 installs `adminguide`;
ensure that category exists before importing administration pages. Baseline
exports preserve their original categories and checksums.

## Keywords

[Keyword assignments and import rules](KEYWORDS.md) cover all authored resources
and the existing baseline articles and cookbooks. Guide preparation also generates
`prepared/keyword-import-plan.json` for the final additive keyword pass.

## Correct captured baseline texts on import

The [baseline revision workflow](baseline/revisions/README.md) supplies final
English texts for existing articles and cookbooks. Guide rendering also generates
`prepared/baseline-import-plan.json`. Apply it **after** baseline seeding and guide
adoption so the corrected merged bodies take precedence. The full Erlang importer applies these corrections last and checks source hashes
and dependencies while preserving identities, translations and unmanaged connections. See the [94-page audit](review/baseline-2026-10-07.md).

## Publication on import

New documentation pages, collections and screenshot resources are published on
import so they can be reviewed on the site. This is recorded in the source front
matter, manifests and prepared resource properties. Preserve publication state
when adopting or updating existing pages, including baseline text revisions.
Preparation itself does not write to or publish anything on a website.

## Sidenotes in guide texts

Use the site's Tufte asides for short supplementary explanations, terminology or
optional background. Place an aside just before the paragraph it annotates, with
blank lines around the block:

```markdown
::: aside
A page can belong to several collections without copying its text.
:::

Add the existing pages to the collection and arrange their reading order.
```

Markdown formatting and documentation references work inside the block. On wide
screens it appears in the margin; on narrow screens it remains visible in the
reading flow. Keep it short and avoid consecutive asides that crowd the margin.
Required steps, prerequisites and warnings belong in the main text. Use ordinary
paragraphs when the explanation is needed to understand or complete the task.
Do not wrap numbered steps or code examples in an aside merely to shorten a page.

## Screenshot captions

The portable importer converts standalone Markdown screenshots into native z-media
embeds, preserving the alt text. A paragraph immediately below the image that
repeats its alt text is treated as its caption and rendered by `_body_media.tpl`
as a `figcaption`, not duplicated as running text. Images without that paragraph
keep their alt text and have no visible caption. Offline Markdown previews still
show the image and paragraph.

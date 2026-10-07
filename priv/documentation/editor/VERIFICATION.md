# Verification notes

## Coverage

The detailed proposal is covered by 93 unique text pages. The access-troubleshooting
page is reused in two collections through two `haspart` edges.

| Collection | Unique pages | Notes |
| --- | ---: | --- |
| Getting started | 5 | Includes the brief introduction to pages and content. |
| Creating and editing content | 11 | Includes titles, reuse, deletion, revisions, and editorial notes. |
| Publishing | 7 | Distinguishes saving, publication period, access, preview, removal, and indexing. |
| Media | 12 | Includes rich-text placement, captions, reuse, replacement, documents, video, and accessibility. |
| Organizing content | 8 | Includes featured content, order, and incoming/outgoing relationships. |
| Menus and navigation | 6 | Includes submenus, removal, and multiple menus. |
| Languages and translations | 7 | Includes adding/removing translations, copying, fallback, and URLs. |
| People and access | 7 | Includes user creation, ownership, collaboration groups, and upload restrictions. |
| Mailing lists and newsletters | 8 | Uses the current review/run workflow, including recipient-language choices. |
| SEO and sharing | 6 | Includes social previews, page paths, and redirects. |
| Common tasks and troubleshooting | 9 | Plus a connection to the existing “Why can’t I edit this page?” page. |
| Understanding Zotonic | 7 | Includes menus/collections and languages as well as the core concepts. |

## Live admin checks

Checked in English on `https://zotonicwww2.test:8443`, 2026-10-06:

- Logged in to the admin using the user-supplied test credentials. Credentials
  are not stored in the bundle.
- Created unpublished Text resource 14584, entered and saved a title, summary,
  and body, and verified the saved content after returning to the page.
- Inspected **Connected to**, **Connected from**, and **Settings & more**.
- Opened **Publication period** and verified the visible field labels. Retook
  the screenshot after the first crop missed the widget. Inspected the saved
  replacement image, not only the browser screenshot output.
- Opened the translation dialog and checked its copy, automatic translation,
  overwrite, and removal wording. No translation service call was made.
- Opened the attached-media upload dialog; no media was uploaded to the test site.
- Created unpublished menu resource 14585, added the example Text page, reloaded
  the menu editor, and verified that the entry persisted without a form save.
- Saved sample SEO title and description on 14584 and captured the fields.
- Sent a single-address test email with the user's authorization and catch-all
  configuration. Run 1 finished with one sent message and zero failures. The
  status page explicitly distinguishes server acceptance from inbox delivery.

The normal site menu and existing content were not changed. No modules needed
enabling. No full-list mailing, scheduled mailing, account creation, deletion,
revision restore, or production write was performed during verification.

## Screenshot inventory

Screenshots are JPEGs captured from the real test admin and
visually inspected after saving. The documentation dashboard/import panel is
excluded. No mock UI or generated replacement text is used in screenshots.

| File | Used for |
| --- | --- |
| `assets/create-page.jpg` | New page category, language, and publication choices. |
| `assets/edit-page.jpg` | Title, summary, save controls, and connection tabs. |
| `assets/publication-period.jpg` | Complete publication-period widget, including all three dates. |
| `assets/upload-media.jpg` | Upload dialog and media-language choice. |
| `assets/edit-menu.jpg` | Sample menu containing an unpublished page. |
| `assets/translate-page.jpg` | Translation methods, source/destination languages, and overwrite choice. |
| `assets/seo-fields.jpg` | SEO title and description. |
| `assets/test-mailing.jpg` | Single-address test email dialog. |
| `assets/mailing-status.jpg` | Completed test run and delivery counts. |

## Source checks

The checked-out source was used to verify features not exercised destructively.
Paths below are relative to the Zotonic workspace root.

| Area | Primary source |
| --- | --- |
| Save/publish/duplicate/delete | `apps/zotonic_mod_admin/priv/templates/_admin_edit_content_publish.tpl` |
| Publication dates | `apps/zotonic_mod_admin/priv/templates/_admin_edit_content_pub_period.tpl` |
| Page settings | `apps/zotonic_mod_admin/priv/templates/_admin_edit_content_acl.tpl` and the live ACL-specific form |
| Notes | `apps/zotonic_mod_admin/priv/templates/_admin_edit_content_note_inner.tpl` |
| URLs and site-search visibility | `apps/zotonic_mod_admin/priv/templates/_admin_edit_content_advanced.tpl` |
| Image captions and alignment | `apps/zotonic_mod_editor_tinymce/priv/templates/_tinymce_dialog_zmedia_props.tpl` |
| Menus | `apps/zotonic_mod_menu/priv/templates/_admin_menu_menu_view.tpl`, `_menu_edit_item.tpl` |
| Translations | `apps/zotonic_mod_translation/priv/templates/_dialog_rsc_language.tpl`, `_translation_edit_languages.tpl` |
| Revision history | `apps/zotonic_mod_backup/priv/templates/_admin_edit_sidebar.tpl` |
| User creation | `apps/zotonic_mod_admin_identity/priv/templates/_action_dialog_user_add.tpl` |
| Mailing selection/review | `apps/zotonic_mod_mailinglist/priv/templates/_dialog_mailing_page.tpl`, `_dialog_mailing_review.tpl` |
| Recipient management | `apps/zotonic_mod_mailinglist/priv/templates/admin_mailinglist_recipients.tpl` |
| Mailing status | `apps/zotonic_mod_mailinglist/priv/templates/_mailing_run_status.tpl`, `_mailing_run_summary.tpl` |
| SEO | `apps/zotonic_mod_seo/priv/templates/_admin_edit_content_seo.tpl` |
| Markdown extension | `apps_user/zotonicwww2/src/support/zotonicwww2_doc_link.erl` |
| Collection navigation | `apps_user/zotonicwww2/priv/templates/page.tpl` |
| Resource/edge HTTP models | `apps/zotonic_core/src/models/m_rsc.erl`, `m_edge.erl` |

## Offline checks

`prepare.py --render` validates all 106 resource documents, the 106 ordered
collection edges, screenshot references, and local links, then renders all
documents through the site's compiled Markdown extension. Technical code-span
references resolve to `/id/doc_…` links. Source heading duplication is removed
from prepared resource bodies.

## Remaining production checks

These are import/deployment checks, not missing editor text:

- Confirm the destination's deployed UI/model versions and content group.
- Resolve any pre-existing `editor_*` names and the existing User Guide entry.
- Upload media, resolve `asset://` references, and verify image accessibility.
- Verify ordered `haspart` edges and production rendering with real resource IDs.
- Publish the reviewed guide and connect it to site navigation with the OAuth key.

Site-specific behavior is identified in the prose where appropriate: block
types, alternative-text fields, collaboration screens, redirect tools, related
content selection, sharing-image selection, and public menu depth. The guide
does not imply that all websites expose identical controls.

## Surveys and forms — 7 October 2026

See the [survey verification record](../review/surveys-verification.md) for the
12 new tasks, three screenshots, and local submission check.

## Additional screenshots — 2026-10-07

See [capture record](../review/screenshots-2026-10-07.md) for the additional real-admin screenshots, their article placements, and verification limits.

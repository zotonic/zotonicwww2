# Additional documentation screenshots — 2026-10-07

Nine additional JPEGs captured from the authenticated local test admin at `https://zotonicwww2.test:8443`. The three bundles now contain 21 screenshots: 16 editor, one developer, and four administration.

Each capture uses the standard admin interface. No dashboard documentation is included. Crops focus on complete controls or dialogs; image pixels and UI text were not altered. Alt text and explanatory captions accompany every new image. Stable media names are registered in the bundle manifests.

| Bundle | Screenshot | Article |
| --- | --- | --- |
| editor | [Image](../editor/assets/change-category.jpg) | [Article](../editor/02-creating-and-editing/change-category.md) |
| editor | [Image](../editor/assets/add-connection.jpg) | [Article](../editor/05-organizing-content/connecting-pages.md) |
| editor | [Image](../editor/assets/survey-response.jpg) | [Article](../editor/10-surveys-and-forms/reviewing-results.md) |
| editor | [Image](../editor/assets/page-revisions.jpg) | [Article](../editor/02-creating-and-editing/revisions.md) |
| developer | [Image](../developer/assets/development-tools.jpg) | [Article](../developer/08-development-tools/development-tools-overview.md) |
| administration | [Image](../administration/assets/languages.jpg) | [Article](../administration/02-site-settings-and-modules/enable-languages.md) |
| administration | [Image](../administration/assets/access-rule-controls.jpg) | [Article](../administration/01-people-and-permissions/access-rules.md) |
| administration | [Image](../administration/assets/upload-permissions.jpg) | [Article](../administration/02-site-settings-and-modules/upload-policy.md) |
| administration | [Image](../administration/assets/start-backup.jpg) | [Article](../administration/05-backups-and-recovery/make-backup.md) |

## Capture checks

- Sample page 14584: opened the category dialog, searched for a related page, and compared two existing revisions. No category, connection, content, or revision was changed.
- Sample survey 14601: opened the existing documentation-test response. No answer, status, or note was changed and no email was sent.
- Viewed language, access-rule, upload-permission, backup, and development settings without saving settings, publishing rules, or starting a backup.
- Backup crop omits the local filesystem path. Survey response uses fictional example contact details.
- These captures verify the visible controls, not a full backup/restore, permission change, or configuration test. Screenshot settings are examples, not recommended production configuration.

## Preparation checks

All three bundle renderers passed: 119 editor resources and 16 screenshots; 160 developer resources and one screenshot; 39 administration resources and four screenshots. All local links and media registrations resolve. The nine documentation review tests and `git diff --check` passed. The future importer still needs to upload the media and resolve `asset://` placeholders.

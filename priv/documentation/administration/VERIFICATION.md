# Administration guide verification — 7 October 2026

Reviewed every new page for its audience, access requirement, prerequisites,
ordered actions, expected result, failure consequences, and related tasks.
The review inventory contains all 38 owned resources; the five shared tasks
retain their existing inventory entries and stable names.

## Source checks

- User creation, group membership, and local username removal: templates in
  `zotonic_mod_admin_identity` and
  `zotonic_mod_acl_user_groups/priv/templates/_admin_edit_basics_user_extra.tpl`.
  Removing a local username is deliberately not described as complete revocation
  of external identities, API tokens, or existing sessions.
- ACL edit/publish distinction: `_admin_acl_rules_publish_buttons.tpl`, including
  **Try rules...**, **Publish**, and **Revert back to published version**.
- Upload policy: `admin_acl_rules_upload.tpl`, including inherited MIME settings,
  group upload size, empty/default values, and `none` semantics.
- Languages: `zotonic_mod_translation/priv/templates/admin_translation.tpl`,
  including View/Editable/Off, default ordering, and the Off warning.
- Backup scope: `_admin_backup_widget_backup_now.tpl` and the existing reviewed
  `zotonic_cmd_backup` documentation, including rotation, cloud file-store
  exclusions, missing tools, encryption keys, and destructive restore/download.
- Mail scope: `z_config.erl` and `z_email_server.erl`, including global and site
  relay keys, destination overrides, and separate queue/delivery outcomes.
- Installation, startup, configuration, and storage use the existing reviewed
  developer tasks as shared tasks or related resources. OTP 28 remains advised.

## Verification limits

The new guide is a source-reviewed draft. No production accounts, permission
rules, mail routing, storage, services, or databases were changed for it. No
complete production deployment or restore has been executed as part of writing
these pages. Existing editor screenshots and earlier local checks do not establish
runtime verification of every administration procedure.

Before publication, walk through account/group changes, ACL trials, language
states, and upload limits on a test site with representative ordinary accounts.
Capture standard-admin screenshots where they clarify these screens; exclude the
zotonicwww2 documentation dashboard. For operating tasks, select the deployment
platform and test service startup, HTTPS/WebSockets, mail delivery, complete
recovery, and rollback with its actual configuration.

## Automated validation

The shared preparer validates all resource metadata, links, graph targets, edge
order, and collection reachability, then renders with the site's Markdown engine.
Cross-guide `haspart` membership is allowed only for authored text resources;
external collection membership is rejected. A focused regression test covers
both outcomes, in addition to the existing graph checks.

All three bundles rendered successfully. All six connection tests passed.
The inventory covers all 317 unique resources, and all generated local preview
links resolve, including shared collection members.

## Media sandbox and runner addition

Added `admin_media_sandbox` after checking `doc/technotes/media-sandboxing.md`,
`z_exec`, `z_media_runner`, `z_media_runner_pool`, and
`controller_media_runner_callback` in the current checkout. This includes the
unsupported-platform behaviour, pool precedence, availability-only fallback,
per-node callbacks, and ImageMagick compatibility. No sandbox settings were
changed and no remote runner was deployed or tested. The runner setup was additionally checked against the local checkout at
`/Users/marc/Sites/zotonic-master-merge/apps_user/mediarunner`: its README,
`docs/docker.md`, `docs/reference.md`, supplied site configuration,
`m_mediarunner_consumer`, and consumer dialogs. This confirms the dedicated
consumer flow, configuration scopes, key rotation, and operating notes. These
are source checks, not a new runner deployment or end-to-end execution test.

## Additional screenshots — 2026-10-07

See [capture record](../review/screenshots-2026-10-07.md) for the additional real-admin screenshots, their article placements, and verification limits.

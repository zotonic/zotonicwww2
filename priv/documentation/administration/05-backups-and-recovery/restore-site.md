---
name: "admin_restore_site"
title: "Rehearse a complete site restore"
summary: "Restore into isolation and verify content, media, and access."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_backups"
order: 3
required_modules: ["mod_backup"]
zotonic_keywords: ["how_to_guide", "operator", "backup_and_restore", "reliability"]
---

# Rehearse a complete site restore

**Access needed:** Server, database, storage, and backup access.

Start with a separate recovery environment. Restoring changes its database and files and makes the destination unavailable; identify that destination before running a restore command.

1. Obtain the selected backup, its encryption credentials if needed, matching code, and required configuration and external media copies.
2. Isolate the destination from public traffic and production integrations. Set controlled test-email delivery before enabling workflows.
3. Use the installation's supported restore procedure. For a local backup known to `mod_backup`, use `bin/zotonic backup garden restore BACKUP_NAME`, replacing both placeholders appropriately.
4. Read the confirmation prompts, including whether configuration and security files will be restored. Check their effect on database hosts, domains, and credentials.
5. Inspect completion logs and the resulting data; the command's exit status alone is not sufficient.
6. Verify an older page, a recent page, an original media file and preview, login, and one representative workflow.
7. Record the recovery point, time taken, missing items, and changes needed to the backup plan.

`backup download` also restores and requires the backup environment. It is not a harmless way to fetch a copy. Read the command reference before choosing it.

Keep recovery testing isolated until copied schedules, email settings, and external service connections have been reviewed.

---
name: "developer_backup_recovery"
title: "Verify backups by restoring them"
summary: "Define what must be recovered: database, original media, configuration, and security material. Check which parts the selected backup includes before depending on it."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 6
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["how_to_guide", "backend_developer", "backup_and_restore", "reliability"]
---

# Verify backups by restoring them

Define what must be recovered: database, original media, configuration, and security material. Check which parts the selected backup includes before depending on it.

Use an isolated destination to restore a backup and verify a page, a media file, and a login or other essential operation. Record the backup's age and the time required to restore it.

The `backup download` command downloads and restores the newest backup; it is not a read-only download. Read the command reference before using restore operations and confirm the destination site.

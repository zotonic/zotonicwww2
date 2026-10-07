---
name: "developer_cli_backups"
title: "Back up and restore site data"
summary: "For a site with backup support enabled:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 15
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "backup_and_restore"]
---

# Back up and restore site data

For a site with backup support enabled:

```sh
bin/zotonic backup garden list
bin/zotonic backup garden start
```

The list is read-only; start requests a new backup. Check the reported status and resulting backup rather than assuming that the request completed synchronously.

Restore is a separate, consequential operation. Confirm the destination site, the selected backup, and its files before running `backup garden restore <backup-name>`. Test recovery on an isolated site first.

The command also has a download operation; consult [backup: List, create, or restore backups](../12-command-reference/cmd-backup.md) for the current behavior. Keep backups and decryption credentials outside the source repository. See [Verify backups by restoring them](../10-configuration-deployment/backup-recovery.md).

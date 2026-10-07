---
name: "developer_cmd_backup"
title: "backup: List, create, or restore backups"
summary: "List, create, or restore backups."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 2
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_backup.erl"]
zotonic_keywords: ["reference", "backend_developer", "backup_and_restore"]
---

# backup: List, create, or restore backups

List, create, or restore backups.

## Syntax

```text
bin/zotonic backup <site_name> list|start|restore|download [backup_name]
```

## Requirements and effects

Requires a running site with mod_backup. Listing is read-only; the other operations change backup or site data.

`list` lists backups; `start` requests a backup. `restore <backup_name>` asks you to type the site name and choose whether to restore configuration and security files; database and files are the default. **`download` downloads and restores the newest backup**, requires the backup environment, and asks for confirmation. Restore makes the destination unavailable. Check completion and restored content afterwards; some failure paths print an error without a failing exit status.

## Example

```sh
bin/zotonic backup garden list
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

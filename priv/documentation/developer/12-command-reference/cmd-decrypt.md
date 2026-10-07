---
name: "developer_cmd_decrypt"
title: "decrypt: Decrypt a local encrypted backup file"
summary: "Decrypt a local encrypted backup file."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 13
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_decrypt.erl"]
zotonic_keywords: ["reference", "backend_developer", "backup_and_restore", "security"]
---

# decrypt: Decrypt a local encrypted backup file

Decrypt a local encrypted backup file.

## Syntax

```text
bin/zotonic decrypt <password> <input_file> [output_file]
```

## Requirements and effects

Operates on local files through backup_file_crypto; it does not restore a site.

The password is a command-line argument and can appear in process listings or shell history. Use an appropriate private environment. Check the resulting output file before a separate restore operation. Errors are printed, so do not assume that exit status alone distinguishes every failure.

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

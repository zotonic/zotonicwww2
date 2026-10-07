---
name: "developer_cmd_configfiles"
title: "configfiles: List selected global configuration files"
summary: "List selected global configuration files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 8
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_configfiles.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# configfiles: List selected global configuration files

List selected global configuration files.

## Syntax

```text
bin/zotonic configfiles 
```

## Requirements and effects

Uses the running node to resolve files when reachable and a local resolution fallback otherwise.

Use this before editing a guessed configuration path. Check the target node shown in the output. A list of selected files does not prove their values are valid or that a running process has re-read a change; use configtest and an appropriate behavior check.

## Example

```sh
bin/zotonic configfiles
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

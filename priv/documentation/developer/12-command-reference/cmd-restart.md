---
name: "developer_cmd_restart"
title: "restart: Restart Zotonic and all sites within the current VM"
summary: "Restart Zotonic and all sites within the current VM."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 22
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_restart.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# restart: Restart Zotonic and all sites within the current VM

Restart Zotonic and all sites within the current VM.

## Syntax

```text
bin/zotonic restart 
```

## Requirements and effects

Requires a running node; affects every hosted site without restarting the Erlang VM.

Use restartsite when only one site needs restarting. Expect interruption while applications and sites restart. Check logs and status afterwards, then make an HTTP request to the relevant site. This is broader than loading changed code or flushing a site cache.

## Example

```sh
bin/zotonic restart
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

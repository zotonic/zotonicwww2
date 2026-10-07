---
name: "developer_cmd_stop"
title: "stop: Stop the Zotonic node"
summary: "Stop the Zotonic node."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 36
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_stop.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# stop: Stop the Zotonic node

Stop the Zotonic node.

## Syntax

```text
bin/zotonic stop 
```

## Requirements and effects

Requires a reachable node and stops all sites hosted by it.

Use stopsite if only one site should stop. Account for active requests and background jobs before stopping a shared node. After a later start, verify site state and the operations that depend on persistent storage. This command is not a way to disconnect a remote shell.

## Example

```sh
bin/zotonic stop
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

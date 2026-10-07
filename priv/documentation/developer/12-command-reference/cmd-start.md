---
name: "developer_cmd_start"
title: "start: Start Zotonic in the background"
summary: "Start Zotonic in the background."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 32
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_start.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# start: Start Zotonic in the background

Start Zotonic in the background.

## Syntax

```text
bin/zotonic start 
```

## Requirements and effects

Starts a node from the selected configuration.

Use status and logs to verify startup. A returned command prompt does not mean every site has finished starting. Use shell to connect to the running node, and a separate HTTP request to confirm the site behavior. Avoid starting a duplicate node with the same identity.

## Example

```sh
bin/zotonic start
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

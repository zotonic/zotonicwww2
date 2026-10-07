---
name: "developer_cmd_startsite"
title: "startsite: Start a selected site on the node"
summary: "Start a selected site on the node."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 34
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_startsite.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# startsite: Start a selected site on the node

Start a selected site on the node.

## Syntax

```text
bin/zotonic startsite <site_name>
```

## Requirements and effects

Requires a running node and a discoverable site application.

Read startup output and check status after requesting the start. Resolve configuration, database, or module initialization errors before retrying. Starting a site is separate from starting the Zotonic node itself; use the node startup command if no node is running.

## Example

```sh
bin/zotonic startsite garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

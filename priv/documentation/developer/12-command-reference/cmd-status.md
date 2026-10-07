---
name: "developer_cmd_status"
title: "status: Show node and site state"
summary: "Show node and site state."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 35
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_status.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# status: Show node and site state

Show node and site state.

## Syntax

```text
bin/zotonic status 
```

## Requirements and effects

Connects to the selected node and reads the site manager state.

Use this to distinguish node availability from individual site states. A running site state does not prove every URL, database query, or external service works. Follow it with a representative HTTP request when checking a deployment or diagnosing a user-visible failure.

## Example

```sh
bin/zotonic status
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

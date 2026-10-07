---
name: "developer_cmd_wait"
title: "wait: Wait until the node answers a ping"
summary: "Wait until the node answers a ping."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 39
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_wait.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# wait: Wait until the node answers a ping

Wait until the node answers a ping.

## Syntax

```text
bin/zotonic wait [timeout_seconds]
```

## Requirements and effects

Connects to the selected node and polls zotonic:ping/0; the default timeout is 30 seconds.

Pass an integer timeout in seconds. A successful ping does not mean every site is ready. The current timeout path prints a message and calls halt without an explicit failure status, so do not treat shell success alone as a reliable readiness condition. Check site state and an HTTP response separately.

## Example

```sh
bin/zotonic wait 30
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

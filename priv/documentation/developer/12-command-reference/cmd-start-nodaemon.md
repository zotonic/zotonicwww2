---
name: "developer_cmd_start_nodaemon"
title: "start_nodaemon: Start Zotonic in the foreground without an interactive shell"
summary: "Start Zotonic in the foreground without an interactive shell."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 33
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_start_nodaemon.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# start_nodaemon: Start Zotonic in the foreground without an interactive shell

Start Zotonic in the foreground without an interactive shell.

## Syntax

```text
bin/zotonic start_nodaemon 
```

## Requirements and effects

Starts a node and keeps the foreground process attached.

Use this mode when a process supervisor or terminal should own the running process without an interactive Erlang shell. Read startup output and verify site readiness separately. Use the shell command from another terminal if you need a remote shell for inspection.

## Example

```sh
bin/zotonic start_nodaemon
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

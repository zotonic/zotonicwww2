---
name: "developer_cmd_debug"
title: "debug: Start Zotonic in the foreground with an Erlang shell"
summary: "Start Zotonic in the foreground with an Erlang shell."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 12
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_debug.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# debug: Start Zotonic in the foreground with an Erlang shell

Start Zotonic in the foreground with an Erlang shell.

## Syntax

```text
bin/zotonic debug 
```

## Requirements and effects

Starts a node using the selected configuration; use it when that node is not already running.

Keep the terminal open while developing and read startup errors there. For a node that is already running, use `shell` instead. The foreground shell belongs to the running VM; commands that terminate that VM also stop the sites it hosts.

## Example

```sh
bin/zotonic debug
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

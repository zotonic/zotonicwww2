---
name: "developer_cmd_update"
title: "update: Compile, load, flush, and rescan the server"
summary: "Compile, load, flush, and rescan the server."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 38
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_update.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# update: Compile, load, flush, and rescan the server

Compile, load, flush, and rescan the server.

## Syntax

```text
bin/zotonic update 
```

## Requirements and effects

Requires a running node and invokes the broad Zotonic update operation.

The update covers compilation and loading of code, cache flushing, and module rescanning. It can affect more than the site you are currently browsing. Inspect build output and module state afterwards. Prefer a targeted command when you only need to diagnose one stage of the development workflow.

## Example

```sh
bin/zotonic update
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

---
name: "developer_cmd_compile"
title: "compile: Request recompilation on the running node"
summary: "Request recompilation on the running node."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 5
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_compile.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# compile: Request recompilation on the running node

Request recompilation on the running node.

## Syntax

```text
bin/zotonic compile 
```

## Requirements and effects

Requires a running node and invokes zotonic_filehandler_compile:all/0.

This is the running-node build workflow. Read subsequent build output to confirm compilation completed successfully. Use `make` for the normal workspace build when the node is not running. A request result alone does not verify that the changed behavior works.

## Example

```sh
bin/zotonic compile
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

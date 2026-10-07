---
name: "developer_cmd_rpc"
title: "rpc: Call an exported Erlang function on the node"
summary: "Call an exported Erlang function on the node."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 24
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_rpc.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# rpc: Call an exported Erlang function on the node

Call an exported Erlang function on the node.

## Syntax

```text
bin/zotonic rpc <module> <function> [argument ...]
```

## Requirements and effects

Requires a running node; effects depend entirely on the called function.

Each argument is passed as a string, not parsed as an Erlang term. A value such as `garden` is therefore not automatically an atom. Use the shell for typed Erlang expressions and context-aware model calls. The no-argument ping example is read-only.

## Example

```sh
bin/zotonic rpc zotonic ping
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

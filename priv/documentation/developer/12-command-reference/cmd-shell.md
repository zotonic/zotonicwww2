---
name: "developer_cmd_shell"
title: "shell: Connect an Erlang shell to the running node"
summary: "Connect an Erlang shell to the running node."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 27
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_shell.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# shell: Connect an Erlang shell to the running node

Connect an Erlang shell to the running node.

## Syntax

```text
bin/zotonic shell 
```

## Requirements and effects

Requires a reachable running node and matching connection configuration.

Choose a site context explicitly with `C = z:c(garden).` before using site models. This is a trusted administrative shell, not a visitor context. Exit the remote shell with Ctrl-C twice. Do not call `q().` to disconnect, because it stops the connected Zotonic node.

## Example

```sh
bin/zotonic shell
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

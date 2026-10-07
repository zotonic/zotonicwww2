---
name: "developer_cmd_setconfig"
title: "setconfig: Set a global runtime Zotonic value"
summary: "Set a global runtime Zotonic value."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 26
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_setconfig.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# setconfig: Set a global runtime Zotonic value

Set a global runtime Zotonic value.

## Syntax

```text
bin/zotonic setconfig zotonic <name> <value>
```

## Requirements and effects

Requires a running node and changes memory configuration without updating files.

The parser converts `true`, `false`, and `undefined` to those atoms. Other values remain strings, so numbers and compound terms are not generally parsed as Erlang values. Check that the target setting supports the supplied type and runtime changes. Persist a lasting change in the proper configuration file.

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

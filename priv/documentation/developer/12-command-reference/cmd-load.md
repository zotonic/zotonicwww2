---
name: "developer_cmd_load"
title: "load: Load changed BEAM files"
summary: "Load changed BEAM files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 17
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_load.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# load: Load changed BEAM files

Load changed BEAM files.

## Syntax

```text
bin/zotonic load 
```

## Requirements and effects

Requires a running node and invokes zotonic_filehandler_compile:ld/0.

The implementation loads changed compiled modules; it does not select a module from an argument despite the short help description. Compile the source first and inspect errors. Reloading code does not necessarily restart processes or rerun their initialization.

## Example

```sh
bin/zotonic load
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

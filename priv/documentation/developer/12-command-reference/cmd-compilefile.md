---
name: "developer_cmd_compilefile"
title: "compilefile: Recompile one Erlang source file"
summary: "Recompile one Erlang source file."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 6
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_compilefile.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# compilefile: Recompile one Erlang source file

Recompile one Erlang source file.

## Syntax

```text
bin/zotonic compilefile <path/to/file.erl>
```

## Requirements and effects

Requires a running node; passes the path to the file handler recompile operation.

Use the actual source path from the workspace. Check the returned result and compiler output. A single-file compilation may not cover changed dependencies or generated assets; use the normal build for those changes and then verify the affected behavior.

## Example

```sh
bin/zotonic compilefile apps_user/garden/src/garden.erl
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

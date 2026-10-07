---
name: "developer_cmd_config"
title: "config: Display configuration resolved from files"
summary: "Display configuration resolved from files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 7
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_config.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# config: Display configuration resolved from files

Display configuration resolved from files.

## Syntax

```text
bin/zotonic config [all|zotonic|erlang]
```

## Requirements and effects

Reads configuration for the selected node; no running node is required for the file display.

With no argument, displays Zotonic configuration. `all` also includes Erlang configuration. This does not query every live runtime override or database-backed module setting. Output can include secrets, so remove credentials before sharing it. Use configfiles to locate the files.

## Example

```sh
bin/zotonic config zotonic
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

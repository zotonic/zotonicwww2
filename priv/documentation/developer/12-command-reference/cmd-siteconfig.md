---
name: "developer_cmd_siteconfig"
title: "siteconfig: Display a site configuration resolved from files"
summary: "Display a site configuration resolved from files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 28
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_siteconfig.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# siteconfig: Display a site configuration resolved from files

Display a site configuration resolved from files.

## Syntax

```text
bin/zotonic siteconfig <site_name>
```

## Requirements and effects

Reads site configuration and merges the relevant global configuration.

Use siteconfigfiles first if you need to locate the inputs. Output can contain credentials and does not represent every database-backed module setting or live process state. After editing a setting, check how its consumer reloads it and verify the affected behavior.

## Example

```sh
bin/zotonic siteconfig garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

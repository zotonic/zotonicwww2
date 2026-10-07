---
name: "developer_cmd_siteconfigfiles"
title: "siteconfigfiles: List configuration files selected for a site"
summary: "List configuration files selected for a site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 29
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_siteconfigfiles.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# siteconfigfiles: List configuration files selected for a site

List configuration files selected for a site.

## Syntax

```text
bin/zotonic siteconfigfiles <site_name>
```

## Requirements and effects

Resolves the site configuration files for the target node locally.

Use this when a change in a guessed file has no effect. Confirm the application and site name before editing a file. Listing files does not validate their content or show whether a running site has adopted a new value.

## Example

```sh
bin/zotonic siteconfigfiles garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

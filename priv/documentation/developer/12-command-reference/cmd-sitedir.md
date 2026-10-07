---
name: "developer_cmd_sitedir"
title: "sitedir: Show a site application directory"
summary: "Show a site application directory."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 30
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_sitedir.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# sitedir: Show a site application directory

Show a site application directory.

## Syntax

```text
bin/zotonic sitedir <site_name>
```

## Requirements and effects

Requires a running node and asks z_path for the site directory.

Use it to verify which application the node resolves for a site. The returned path can be a build or linked application path; inspect its relationship to your source checkout before editing. A path alone does not indicate that the site is running successfully.

## Example

```sh
bin/zotonic sitedir garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

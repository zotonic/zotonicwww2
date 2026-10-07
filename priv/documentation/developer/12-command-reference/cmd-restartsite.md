---
name: "developer_cmd_restartsite"
title: "restartsite: Restart one site"
summary: "Restart one site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 23
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_restartsite.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# restartsite: Restart one site

Restart one site.

## Syntax

```text
bin/zotonic restartsite <site_name>
```

## Requirements and effects

Requires a running node and interrupts the selected site.

Use this when a change requires the site lifecycle to run again. Check startup logs and site status afterwards. A successful command response is not a full readiness check; load a representative page and verify any configuration-dependent operation you changed.

## Example

```sh
bin/zotonic restartsite garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

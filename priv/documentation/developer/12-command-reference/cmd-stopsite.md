---
name: "developer_cmd_stopsite"
title: "stopsite: Stop one site"
summary: "Stop one site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 37
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_stopsite.erl"]
zotonic_keywords: ["reference", "backend_developer", "site_management"]
---

# stopsite: Stop one site

Stop one site.

## Syntax

```text
bin/zotonic stopsite <site_name>
```

## Requirements and effects

Requires a running node; other sites on the node remain managed separately.

Confirm the site name and account for requests and jobs using it. Check status after the stop. Use startsite to start it again and verify a real request afterwards. For a code change that only needs loading, use the narrower development workflow.

## Example

```sh
bin/zotonic stopsite garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

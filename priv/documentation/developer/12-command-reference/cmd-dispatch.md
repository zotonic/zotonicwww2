---
name: "developer_cmd_dispatch"
title: "dispatch: Inspect dispatch rules or trace a URL"
summary: "Inspect dispatch rules or trace a URL."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 14
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_dispatch.erl"]
zotonic_keywords: ["reference", "backend_developer", "routing_and_redirects", "dispatch_rule"]
---

# dispatch: Inspect dispatch rules or trace a URL

Inspect dispatch rules or trace a URL.

## Syntax

```text
bin/zotonic dispatch <site_name> [path|detail]
bin/zotonic dispatch <URL>
```

## Requirements and effects

Requires a running node and reads its dispatch information.

A site argument lists rules; `detail` includes options and hostname information. A path traces routing within the site, while a full HTTP or HTTPS URL also supplies a hostname. This inspects dispatch and does not perform an ordinary end-to-end HTTP request.

## Example

```sh
bin/zotonic dispatch garden /welcome
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

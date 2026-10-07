---
name: "developer_cmd_flush"
title: "flush: Flush site caches"
summary: "Flush site caches."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 16
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_flush.erl"]
zotonic_keywords: ["reference", "backend_developer", "cache"]
---

# flush: Flush site caches

Flush site caches.

## Syntax

```text
bin/zotonic flush [site_name]
```

## Requirements and effects

Requires a running node; without a site argument the operation affects all sites.

Specify the site when diagnosing a local problem. Recheck the request after flushing. If this resolves stale output, investigate cache dependencies and keys rather than adding routine flushes to application code. It does not substitute for compiling changed Erlang source.

## Example

```sh
bin/zotonic flush garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

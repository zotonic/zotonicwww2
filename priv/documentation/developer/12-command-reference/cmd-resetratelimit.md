---
name: "developer_cmd_resetratelimit"
title: "resetratelimit: Reset a site rate limiter"
summary: "Reset a site rate limiter."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 21
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_resetratelimit.erl"]
zotonic_keywords: ["reference", "backend_developer", "authentication", "security"]
---

# resetratelimit: Reset a site rate limiter

Reset a site rate limiter.

## Syntax

```text
bin/zotonic resetratelimit <site_name>
```

## Requirements and effects

Requires a running site with mod_ratelimit active.

Use this when a local test deliberately triggered a limit and the test needs to start again. It changes rate-limit state for the site; confirm the site before running it. Inspect the output for missing-site or missing-module errors before retrying the operation.

## Example

```sh
bin/zotonic resetratelimit garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

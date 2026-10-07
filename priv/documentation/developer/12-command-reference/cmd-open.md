---
name: "developer_cmd_open"
title: "open: Open the site URL in a browser"
summary: "Open the site URL in a browser."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 19
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_open.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# open: Open the site URL in a browser

Open the site URL in a browser.

## Syntax

```text
bin/zotonic open <site_name>
```

## Requirements and effects

Requires a running node and browser support on its host.

On macOS the helper uses the default browser; other supported setups use Chrome. The command accepts a single site name. Inspect the loaded hostname and response after opening it. Browser startup is separate from the application being healthy or the user being signed in.

## Example

```sh
bin/zotonic open garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

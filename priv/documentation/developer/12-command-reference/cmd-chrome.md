---
name: "developer_cmd_chrome"
title: "chrome: Open a site in Chrome"
summary: "Open a site in Chrome."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 3
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_chrome.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# chrome: Open a site in Chrome

Open a site in Chrome.

## Syntax

```text
bin/zotonic chrome [-s] [--browser-switch ...] <site_name>
```

## Requirements and effects

Requires a running node and an installed supported browser on the host executing the browser action.

Put browser switches before the final site name. `-s` is a flag without a value in the parser; it enables the local certificate-related behavior. Use the ordinary invocation first. A browser opening successfully does not verify the page response or application behavior.

## Example

```sh
bin/zotonic chrome garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

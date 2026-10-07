---
name: "developer_cmd_chromium"
title: "chromium: Open a site in Chromium"
summary: "Open a site in Chromium."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 4
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_chromium.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# chromium: Open a site in Chromium

Open a site in Chromium.

## Syntax

```text
bin/zotonic chromium [-s] [--browser-switch ...] <site_name>
```

## Requirements and effects

Uses the Chrome command parser and requires a running node and Chromium on the browser host.

Put the site name last. Browser switches start with `--`; `-s` takes no value and enables the certificate-related development behavior. Check the page in the opened browser, including the selected hostname and any redirect. See the chrome reference for the shared argument behavior.

## Example

```sh
bin/zotonic chromium garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

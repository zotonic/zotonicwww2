---
name: "developer_cmd_logtail"
title: "logtail: Print recent lines from a configured log file"
summary: "Print recent lines from a configured log file."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 18
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_logtail.erl"]
zotonic_keywords: ["reference", "backend_developer", "logging_and_monitoring"]
---

# logtail: Print recent lines from a configured log file

Print recent lines from a configured log file.

## Syntax

```text
bin/zotonic logtail [error|crash]
```

## Requirements and effects

Reads the configured local log directory and invokes tail.

With no selector it reads console.log. `error` selects error.log and `crash` selects crash.log. It prints the last 500 lines and exits; it does not follow the file continuously. Confirm the timestamp and site before interpreting an old entry as the current failure.

## Example

```sh
bin/zotonic logtail error
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

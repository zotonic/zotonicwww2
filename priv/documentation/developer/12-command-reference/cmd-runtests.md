---
name: "developer_cmd_runtests"
title: "runtests: Run selected core test modules"
summary: "Run selected core test modules."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 25
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# runtests: Run selected core test modules

Run selected core test modules.

## Syntax

```text
bin/zotonic runtests [test_module ...]
```

## Requirements and effects

Starts the configured test node through the launcher test workflow.

The implementation discovers Erlang test files under `apps/zotonic_*/test`. Arguments select exact discovered module names, without `.erl`; no arguments select all discovered tests. It does not automatically scan every apps_user test directory. Check test selection and output carefully: an unmatched name can select no tests.

## Example

```sh
bin/zotonic runtests
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

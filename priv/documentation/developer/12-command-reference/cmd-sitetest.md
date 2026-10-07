---
name: "developer_cmd_sitetest"
title: "sitetest: Run site tests using an isolated test schema"
summary: "Run site tests using an isolated test schema."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 31
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_sitetest.erl"]
zotonic_keywords: ["reference", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# sitetest: Run site tests using an isolated test schema

Run site tests using an isolated test schema.

## Syntax

```text
bin/zotonic sitetest <site_name>
```

## Requirements and effects

Requires a running node and a disposable testing environment; interrupts the selected site.

The runner stops the site, overrides its schema to `z_sitetest`, drops that schema, starts the site, and runs discovered `*_sitetest` EUnit modules. It then removes the override and restarts with the normal schema. Do not run concurrent tests sharing that database schema. Inspect failures and restoration of normal state.

## Example

```sh
bin/zotonic sitetest garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

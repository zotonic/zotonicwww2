---
name: "developer_cmd_connectdb"
title: "connectdb: Test the configured PostgreSQL connection"
summary: "Test the configured PostgreSQL connection."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 10
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_connectdb.erl"]
zotonic_keywords: ["reference", "backend_developer", "database", "postgresql"]
---

# connectdb: Test the configured PostgreSQL connection

Test the configured PostgreSQL connection.

## Syntax

```text
bin/zotonic connectdb 
```

## Requirements and effects

Reads global database configuration and uses the configured database driver to test the connection.

This command **does not open psql** and accepts no site selection. It prints connection options, including the password, before testing. Keep the output private. A site can override global database settings; a successful result is not proof that its own connection and schema are correct.

## Example

```sh
bin/zotonic connectdb
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

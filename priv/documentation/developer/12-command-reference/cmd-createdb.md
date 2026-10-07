---
name: "developer_cmd_createdb"
title: "createdb: Prepare a site database and schema"
summary: "Prepare a site database and schema."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 11
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_createdb.erl"]
zotonic_keywords: ["reference", "backend_developer", "database", "postgresql"]
---

# createdb: Prepare a site database and schema

Prepare a site database and schema.

## Syntax

```text
bin/zotonic createdb <site_name>
```

## Requirements and effects

Requires a running node and calls z_db:prepare_database/1 for the selected site.

This command can create database storage. Check the destination site configuration and database privileges first. Read the returned result before starting the site; preparing the database alone does not prove that module installation or the full site startup will succeed.

## Example

```sh
bin/zotonic createdb garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

---
name: "developer_cmd_configtest"
title: "configtest: Check whether global configuration files can be read"
summary: "Check whether global configuration files can be read."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 9
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_configtest.erl"]
zotonic_keywords: ["reference", "backend_developer", "configuration"]
---

# configtest: Check whether global configuration files can be read

Check whether global configuration files can be read.

## Syntax

```text
bin/zotonic configtest 
```

## Requirements and effects

Resolves and reads configuration locally for the selected node.

A successful result confirms that the files can be read and parsed. It does not test the database, site startup, storage permissions, or HTTP readiness. Follow a configuration change with a check of the component that consumes it.

## Example

```sh
bin/zotonic configtest
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

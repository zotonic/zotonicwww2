---
name: "developer_inspect_configuration"
title: "Inspect global and site configuration"
summary: "Use the command suited to the question:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 9
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "configuration"]
---

# Inspect global and site configuration

Use the command suited to the question:

```sh
bin/zotonic configfiles
bin/zotonic configtest
bin/zotonic siteconfigfiles garden
bin/zotonic siteconfig garden
```

`configfiles` and `siteconfigfiles` locate configuration sources. `configtest` checks global configuration loading. `siteconfig` prints the site's file-based configuration; it is not a complete dump of every database-backed module setting.

For runtime values, use the relevant model or configuration API in the running context. `setconfig` changes global runtime Zotonic settings without writing configuration files.

Do not share unredacted configuration output: database passwords and other secrets may be present. See [Find the configuration layer to change](../10-configuration-deployment/configuration-layers.md) and [Change a runtime setting deliberately](../10-configuration-deployment/runtime-config.md).

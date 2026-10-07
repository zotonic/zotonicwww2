---
name: "developer_runtime_config"
title: "Change a runtime setting deliberately"
summary: "Use bin/zotonic setconfig only for a global setting that supports a runtime change. The command changes the running configuration and does not save the value in configuration files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 2
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["how_to_guide", "backend_developer", "configuration", "site_management"]
---

# Change a runtime setting deliberately

Use `bin/zotonic setconfig` only for a global setting that supports a runtime change. The command changes the running configuration and does not save the value in configuration files.

Record the previous value and why the change is needed. Verify the affected behavior, then either restore the value or make the intended persistent change in the correct configuration file.

The command converts `true`, `false`, and `undefined` specially; other values are passed as strings. It is not a general Erlang term parser.

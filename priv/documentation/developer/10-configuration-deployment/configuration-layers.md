---
name: "developer_configuration_layers"
title: "Find the configuration layer to change"
summary: "Distinguish global Zotonic configuration, Erlang runtime configuration, site configuration files, and module settings stored for a site. Similar setting names can exist at different layers."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 1
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["explanation", "backend_developer", "configuration", "site_management"]
---

# Find the configuration layer to change

Distinguish global Zotonic configuration, Erlang runtime configuration, site configuration files, and module settings stored for a site. Similar setting names can exist at different layers.

Use `bin/zotonic configfiles` and `bin/zotonic siteconfigfiles garden` to locate the files that the command resolves. Use the corresponding configuration display commands to inspect the values, taking care when sharing output that may include credentials.

Check the implementation of a setting to see when it is read. Some values are consulted on each request; others are used at process or site startup.

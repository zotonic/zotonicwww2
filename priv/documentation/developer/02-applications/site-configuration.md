---
name: "developer_site_configuration"
title: "Site configuration and enabled modules"
summary: "A site is identified by its application and priv/zotonic_site configuration. Supported formats include Erlang .config, JSON, and YAML; follow the format already used by the project."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 7
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["how_to_guide", "backend_developer", "configuration", "site"]
---

# Site configuration and enabled modules

A site is identified by its application and `priv/zotonic_site` configuration. Supported formats include Erlang `.config`, JSON, and YAML; follow the format already used by the project.

For Erlang configuration, the file is a list of terms ending with a period:

```erlang
[
    {enabled, true},
    {environment, development},
    {hostname, "garden.test"}
].
```

This is a small fragment illustrating syntax, not a replacement for the generated database and module settings.

Inspect the actual configuration sources with `siteconfigfiles garden` and `siteconfig garden`. A site's installation module list and its current activation state are different concerns, especially on database-backed sites. Verify enabled modules in the admin or module manager.

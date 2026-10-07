---
name: "developer_module_anatomy"
title: "Anatomy of a reusable module"
summary: "A reusable module normally lives in apps_user/zotonic_mod_garden and contains:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 4
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# Anatomy of a reusable module

A reusable module normally lives in `apps_user/zotonic_mod_garden` and contains:

```text
zotonic_mod_garden/
├── rebar.config
├── src/
│   ├── zotonic_mod_garden.app.src
│   ├── mod_garden.erl
│   └── models/m_garden.erl
└── priv/
    └── templates/
```

The package name is `zotonic_mod_garden`; the module activated on a site is `mod_garden`. Use a `.app.src` whose application name matches the package.

Add controllers, dispatch rules, translations, and assets only when the feature requires them. A template-only feature does not need a custom process. A background service needs an explicit lifecycle and supervision design.

Build the package, make it discoverable, then activate it for the intended site. See [Create a reusable module](../06-reusable-functionality/create-module.md) and [Application startup and per-site module activation](startup-activation.md).

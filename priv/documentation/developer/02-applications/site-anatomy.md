---
name: "developer_site_anatomy"
title: "Anatomy of a site application"
summary: "A typical site contains the following directories. Create only the extension directories you actually need."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 3
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# Anatomy of a site application

A typical site contains the following directories. Create only the extension directories you actually need.

```text
garden/
├── rebar.config
├── src/
│   ├── garden.app.src
│   ├── garden.erl
│   ├── controllers/
│   ├── models/
│   ├── filters/
│   ├── scomps/
│   ├── actions/
│   ├── validators/
│   └── support/
└── priv/
    ├── zotonic_site.config
    ├── dispatch/
    ├── templates/
    ├── lib/
    ├── lib-src/
    └── translations/
```

`garden.erl` contains the site's Zotonic module declarations and lifecycle hooks. A generated site can also have an OTP application callback and supervisor when created with that option.

Keep template-facing business operations in models and focused implementation helpers in `support`. Do not turn the main site module into a collection of unrelated functions. See [What belongs in src and priv](source-directories.md).

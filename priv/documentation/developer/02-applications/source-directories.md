---
name: "developer_source_directories"
title: "What belongs in src and priv"
summary: "Use src for compiled Erlang code and priv for application resources."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 6
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# What belongs in src and priv

Use `src` for compiled Erlang code and `priv` for application resources.

Put request handling in `controllers`, model APIs in `models`, template transformations in `filters`, rendered components in `scomps`, and supporting business logic in `support`. Directory names help readers; the Erlang module name and exported callbacks still determine behavior.

Under `priv`, put routes in `dispatch`, templates in `templates`, browser-ready files in `lib`, and their editable build sources in `lib-src`. Translation catalogs belong in `translations`.

Do not put uploaded files, caches, or local secrets in a deployable assets directory. Use the site's configured storage and configuration mechanisms. Do not edit generated CSS when its source and build command are available.

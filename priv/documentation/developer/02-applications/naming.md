---
name: "developer_naming"
title: "Application and module names"
summary: "Use distinct, consistent names so code and assets can be found without ambiguity."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 5
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# Application and module names

Use distinct, consistent names so code and assets can be found without ambiguity.

For a reusable feature:

- Directory/application: `zotonic_mod_garden`
- Main Zotonic module: `mod_garden`
- Model: `m_garden`
- Controller: `controller_garden_export`
- Filter: `filter_garden_label`
- Site-specific template: `garden_overview.tpl`

Erlang module names are global within a VM. Two sites cannot load different implementations under the same Erlang module name and expect isolation.

Actions, validators, and scomps use Zotonic's naming conventions, including the owning site/module component. Inspect an existing implementation before naming a new one. Templates can intentionally share names to participate in module-priority overrides; ordinary helper Erlang modules should not.

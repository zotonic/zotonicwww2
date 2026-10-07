---
name: "developer_workspace"
title: "The Zotonic workspace"
summary: "The checkout is an Erlang umbrella project: one build contains many applications."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 1
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "module", "site"]
---

# The Zotonic workspace

The checkout is an Erlang umbrella project: one build contains many applications.

- `apps/` contains Zotonic's core applications and bundled modules.
- `apps_user/` contains your sites and additional modules.
- `apps_user/<project>/apps/` can contain applications in a nested project; this path is included in the current build configuration.
- `_build/` contains build output and dependencies. Do not treat it as the authoritative source directory.
- `doc/` contains versioned documentation and reference sources.
- `bin/zotonic` is the command-line entry point.

A site or external module can be its own Git repository inside `apps_user`. Before changing files, check which repository owns them. Add dependencies in the appropriate `rebar.config`, then build from the umbrella root.

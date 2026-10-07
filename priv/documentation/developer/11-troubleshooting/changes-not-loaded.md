---
name: "developer_changes_not_loaded"
title: "A saved change does not appear"
summary: "Check each stage in order: the edited file, build output, loaded application, active module, selected template, and browser response."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 4
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "development_and_debugging"]
---

# A saved change does not appear

Check each stage in order: the edited file, build output, loaded application, active module, selected template, and browser response.

For Erlang, resolve compilation errors and confirm changed BEAM files were loaded. For assets, inspect the compiled output in `priv/lib`. For templates, use the selection tool to verify the exact file used by the page.

Only then investigate caching. A cache flush cannot make an inactive module's template win. Reproduce the result with one small visible change and remove temporary debug output afterwards.

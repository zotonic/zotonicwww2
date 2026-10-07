---
name: "developer_recompile_rescan"
title: "Recompile code and refresh discovery"
summary: "Use compilation for changed Erlang source, loading for changed BEAM files, and an index refresh when new templates or application files are not discovered."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 13
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging"]
---

# Recompile code and refresh discovery

Use compilation for changed Erlang source, loading for changed BEAM files, and an index refresh when new templates or application files are not discovered.

`bin/zotonic compile` requests compilation on the running node. `bin/zotonic load` loads changed BEAM files. `bin/zotonic update` performs a broader update; consult its reference before using it as a routine shortcut.

Read build errors before retrying. Once the build succeeds, inspect the active module and selected template to confirm that the running site sees the change.

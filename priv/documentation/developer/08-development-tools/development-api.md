---
name: "developer_development_api"
title: "Understand the optional development API"
summary: "The Development page has an option to enable an API for recompiling and rebuilding Zotonic. The model implements actions for recompiling, flushing caches, and refreshing indexes and translations."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 14
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging"]
---

# Understand the optional development API

The Development page has an option to enable an API for recompiling and rebuilding Zotonic. The model implements actions for recompiling, flushing caches, and refreshing indexes and translations.

These calls cause changes even though they are exposed through model GET paths. They are intended for a controlled development setup and can be available without authentication when enabled. Do not enable them just to use the admin tools or ordinary command-line commands.

If an integration needs this API, inspect the current `m_development` implementation and constrain access to the development environment. Disable it when the integration is no longer needed.

---
name: "developer_cache_debugging"
title: "Distinguish stale cache data from stale code"
summary: "First check which file is selected and whether the changed code compiled. Then repeat the request with the relevant cache disabled or flushed."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 11
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "cache", "performance"]
---

# Distinguish stale cache data from stale code

First check which file is selected and whether the changed code compiled. Then repeat the request with the relevant cache disabled or flushed.

The Development page can disable `{% cache %}` template blocks. The command `bin/zotonic flush garden` clears site caches more broadly. Use a targeted experiment so you can tell which change affected the result.

If flushing fixes the symptom, inspect cache keys and dependencies. Repeatedly flushing is not a replacement for correct invalidation. Re-enable normal caching before measuring performance and before finishing the investigation.

---
name: "developer_observer_tools"
title: "Find registered observers"
summary: "Open Show an overview of all observers from the Development page. Find the notification and inspect the registered callbacks and their order."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 8
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "notification"]
---

# Find registered observers

Open **Show an overview of all observers** from the Development page. Find the notification and inspect the registered callbacks and their order.

If your callback is absent, check that the module is active, the callback is exported, and the compiled module contains your change. If it is present but has no visible effect, check the notification contract and which caller emits it.

Use a focused trace or log entry to follow one event. Do not add a second observer registration to compensate for an inactive module.

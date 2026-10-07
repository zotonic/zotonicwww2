---
name: "developer_database_trace"
title: "Trace database queries for your session"
summary: "Enable module#mod_server_storage if the Development page says it is needed for database tracing. On the Development page, enable Trace all database queries for the current session."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 9
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "database", "logging_and_monitoring", "postgresql"]
---

# Trace database queries for your session

Enable `module#mod_server_storage` if the Development page says it is needed for database tracing. On the Development page, enable **Trace all database queries for the current session**.

Reproduce one page load or action in that session and inspect the trace output. Look for repeated queries, unexpectedly large result sets, and queries that dominate the request time. A trace from another session may not include your request.

Turn tracing off when finished. Use the evidence to choose a query to investigate; do not add indexes based on query count alone.

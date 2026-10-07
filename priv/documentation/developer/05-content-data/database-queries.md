---
name: "developer_database_queries"
title: "Add a database query when models are not enough"
summary: "Prefer Zotonic models for resources and edges. Use z_db for application tables or queries that the model APIs do not cover."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 10
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "database", "postgresql", "query"]
---

# Add a database query when models are not enough

Prefer Zotonic models for resources and edges. Use `z_db` for application tables or queries that the model APIs do not cover.

Pass parameters separately from SQL text. Never interpolate a request value into SQL. Use a transaction when several database changes must succeed or fail together, and handle transaction failures explicitly.

Inspect the query plan and row count before adding indexes. Test realistic data sizes and keep site separation intact when working with schemas. Database access does not automatically apply resource visibility checks to arbitrary SQL.

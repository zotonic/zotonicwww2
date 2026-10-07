---
name: "developer_resource_identifiers"
title: "Use stable resource names"
summary: "A resource has a numeric ID within its database. Give application-owned resources a stable name when code needs to find them across installations."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 2
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["explanation", "backend_developer", "resource", "identifier"]
---

# Use stable resource names

A resource has a numeric ID within its database. Give application-owned resources a stable name when code needs to find them across installations.

```erlang
Id = m_rsc:rid(page_home, C).
```

Check the result before using it: that name may not exist on your site. A database ID copied from another site can identify unrelated content. Use a stable name, URI, or explicit import mapping when moving data between sites.

Reserve names for resources your application can identify consistently. Editors can still manage their titles and bodies. See `model#rsc`, [Install initial content with a datamodel](datamodel-fixtures.md), and [Move content between sites](import-export.md).

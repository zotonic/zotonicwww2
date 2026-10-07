---
name: "developer_content_edges"
title: "Connect resources and preserve order"
summary: "An edge connects a subject resource to an object resource through a predicate. Use model#edge to inspect and manage these connections."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 5
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "edge", "predicate", "content_relationships"]
---

# Connect resources and preserve order

An edge connects a subject resource to an object resource through a predicate. Use `model#edge` to inspect and manage these connections.

For example, in a local shell:

```erlang
m_edge:objects(Id, haspart, C).
```

Use the edge API for insertions, deletions, and ordering. Direct database writes skip normal notifications and cache invalidation. Check permissions on the requested relationship as well as the resource being edited.

When importing a collection, preserve the order of its children explicitly. A set of correct connections in the wrong order can still produce the wrong navigation. See [Move content between sites](import-export.md) and [Install initial content with a datamodel](datamodel-fixtures.md).

For two existing, authorized resources, connect a guide to its first task:

```erlang
{ok, _EdgeId} = m_edge:insert(GuideId, haspart, TaskId, C).
m_edge:objects(GuideId, haspart, C).
```

The pattern match is useful in a test shell; application code must handle `{error, Reason}`. Use `relation` for related tasks and a site's `hasreference` predicate for citations, when that predicate exists. Neither should change collection membership.

`m_edge:set_sequence(GuideId, haspart, OrderedIds, C)` sets the requested outgoing list; it can add or remove membership as well as reorder it. Read the current list first and supply the complete intended list. Verify it with `m_edge:objects/3`. Do not use this operation to overwrite connections owned by editors during a routine import.

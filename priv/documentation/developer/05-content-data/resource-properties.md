---
name: "developer_resource_properties"
title: "Read and update resource properties"
summary: "Use model#rsc for resource access and m_rsc_update for writes. These APIs handle behavior that a direct SQL update would bypass."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 3
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["explanation", "backend_developer", "resource", "structured_data"]
---

# Read and update resource properties

Use `model#rsc` to read resources and `m_rsc_update` for writes. Use the supplied request context so the operation keeps the caller's site and permissions.

In an authorized local test context, with an existing `garden_welcome` resource:

```erlang
Id = m_rsc:rid(garden_welcome, C).
m_rsc:is_visible(Id, C).
m_rsc:p(Id, title, C).
m_rsc_update:update(Id, #{<<"summary">> => <<"Visit the garden on Saturday.">>}, C).
```

Check that `Id` is an integer before continuing. Expect `{ok, Id}` from a successful update; `{error, Reason}` is a failed write, not a saved page. Read the summary back and check the rendered page. The ordinary `z:c(garden)` shell context does not grant editing permission.

In application code, branch on visibility before returning private properties and on the update result before reporting success. The write API checks access, but you must still select and validate the fields the caller may change. Do not copy every submitted field into the resource or update the SQL table directly.

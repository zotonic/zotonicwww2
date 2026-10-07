---
name: "developer_access_control"
title: "Keep permission checks at the boundary"
summary: "Use the request context when checking access to data and actions. A hidden button is not an authorization check: callers can invoke a model or endpoint directly."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 9
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "authorization_and_access_control", "security"]
---

# Keep permission checks at the boundary

Use the request context when checking access to data and actions. A hidden button is not an authorization check: callers can invoke a model or endpoint directly.

Check the resource, action, and module permission that the operation requires. Return a clear error when access is denied. Test as an anonymous visitor, an editor with limited rights, and an administrator.

Avoid replacing the caller's context with a privileged shell context in application code. If a background task needs elevated access, constrain the operation and authorize its initiation explicitly.

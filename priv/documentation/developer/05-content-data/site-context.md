---
name: "developer_site_context"
title: "Work with a site context"
summary: "A context carries the site and request-related state used by Zotonic APIs. Pass it through your functions instead of storing one context globally."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 1
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["explanation", "backend_developer", "site", "authorization_and_access_control"]
---

# Work with a site context

A context carries the site and request-related state used by Zotonic APIs. Pass it through your functions instead of storing one context globally.

In a trusted local shell, select the site explicitly:

```erlang
C = z:c(garden).
z_context:site(C).
```

This creates a fresh context; it does not automatically log in an administrator. It does not reproduce a visitor's session or permissions. In a model, controller, or event handler, use the supplied request context and preserve it when the API returns an updated context.

Background work needs a deliberate context and authorization policy. See [Inspect content from the Erlang shell](../03-command-line-workflows/shell-data.md), [Keep permission checks at the boundary](access-control.md), and [Run work outside a request](../06-reusable-functionality/background-work.md).

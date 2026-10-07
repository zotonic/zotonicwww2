---
name: "developer_http_api"
title: "Expose an operation through an API"
summary: "Start from the site's existing model API or controller patterns. Specify the method, input fields, response shape, and errors before connecting a browser or external client."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 2
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "api_and_integration", "http", "oauth_2_0"]
---

# Expose an operation through an API

Start from the site's existing model API or controller patterns. Specify the method, input fields, response shape, and errors before connecting a browser or external client.

Use a read operation for retrieval and an appropriate write operation for changes. Apply authentication and authorization to each operation. Test malformed input and access denial as well as a successful response.

Keep transport details out of shared domain functions so the same operation can serve a template, event handler, or API safely. See `module#mod_oauth2`, [Expose data through a model](../06-reusable-functionality/model-api.md), and [Keep permission checks at the boundary](../05-content-data/access-control.md).

The greeting model provides a read-only first check. On the site's hostname, request:

```text
GET /api/model/garden/get/greeting
```

Expect a JSON success response with the greeting as its result. Confirm the HTTP status and response body. Request an unknown path too; the model should return its `unknown_path` error. This uses the public example from the model task, so no token is required for that particular value.

For protected operations, use the authentication mechanism supported by `controller#controller_api`, for example an appropriately scoped OAuth token. Authentication identifies the caller; the model still must authorize access. Keep tokens out of URLs and example files. Read the controller reference for response envelopes, HTTP methods and request-body handling before adding writes.

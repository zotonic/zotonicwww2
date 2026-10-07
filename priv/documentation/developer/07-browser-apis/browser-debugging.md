---
name: "developer_browser_debugging"
title: "Diagnose a failed browser interaction"
summary: "Reproduce one action with the browser console and network panel open. Check for a JavaScript error before the request, a failed request, or a successful response that the page does not apply."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 6
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "development_and_debugging", "javascript", "cotonic"]
---

# Diagnose a failed browser interaction

Reproduce one action with the browser console and network panel open. Check for a JavaScript error before the request, a failed request, or a successful response that the page does not apply.

Verify the element ID, wire attachment, delegate, and expected event. For messaging, inspect the topic and payload and confirm the connection is established.

Then match the request time with the server log. A browser message saying that a request failed does not explain whether validation, permissions, or server code caused it. See [Write logs that explain an operation](../08-development-tools/structured-logging.md), [A model returns an error or no value](../11-troubleshooting/model-errors.md), and [Handle a postback on the server](postback-handlers.md).

---
name: "developer_slow_requests"
title: "A page is slow"
summary: "Measure one representative request and identify whether time is spent in the browser, network, or server. Compare an initial request with a repeated request to see the effect of caches."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 8
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "performance", "logging_and_monitoring"]
---

# A page is slow

Measure one representative request and identify whether time is spent in the browser, network, or server. Compare an initial request with a repeated request to see the effect of caches.

On the server, inspect database queries and structured logs before tracing broad sets of functions. Look for repeated work and expensive calls on the request path. Use a small function trace only after narrowing the likely cause.

Apply one change and repeat the same measurement with normal caching settings. Keep the content, role, and request comparable.

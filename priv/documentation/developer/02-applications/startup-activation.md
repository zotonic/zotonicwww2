---
name: "developer_startup_activation"
title: "Application startup and per-site module activation"
summary: "OTP application startup concerns the application and its processes. Zotonic module activation concerns the feature within an individual site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 10
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["how_to_guide", "backend_developer", "module_management", "site_management"]
---

# Application startup and per-site module activation

OTP application startup concerns the application and its processes. Zotonic module activation concerns the feature within an individual site.

A module can be compiled and available to the VM but inactive on `garden`. Conversely, activating a module can start per-site behavior, attach observers, and trigger installation or schema work. Treat activation as a state-changing operation.

Use an OTP supervisor for application-level services when appropriate. Use the site's/module's supported lifecycle for state that belongs to one site. Pass the correct site context rather than storing a single global site value in a shared process.

When debugging startup, distinguish a missing application, a failed site, an inactive module, and a crashing worker. Each has a different fix. See [Run work outside a request](../06-reusable-functionality/background-work.md).

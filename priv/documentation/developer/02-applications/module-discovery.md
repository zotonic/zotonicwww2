---
name: "developer_module_discovery"
title: "How Zotonic finds templates, dispatch rules, and assets"
summary: "Zotonic indexes resources from the site's active modules. A file existing on disk is not enough: its application must be discoverable, and its module must participate in that site."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 9
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["how_to_guide", "backend_developer", "module_management", "module"]
---

# How Zotonic finds templates, dispatch rules, and assets

Zotonic indexes resources from the site's active modules. A file existing on disk is not enough: its application must be discoverable, and its module must participate in that site.

1. Confirm that the application is included by the umbrella build.
2. Compile it and inspect any `.app.src` errors.
3. Rescan modules when adding a package.
4. Activate its Zotonic module for the site.
5. Check template selection or dispatch inspection for the specific resource.

File watching normally picks up edits and additions in known applications. Adding a whole application can need a rescan or broader update. Do not assume the browser cache is responsible until the server selects the correct source file.

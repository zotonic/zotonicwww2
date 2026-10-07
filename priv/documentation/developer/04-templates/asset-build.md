---
name: "developer_asset_build"
title: "Build and serve frontend assets"
summary: "Keep source files such as SCSS in priv/lib-src and compiled assets in priv/lib. Use an app-level Makefile that delegates asset work to the source directory's Makefile."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 11
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "development_and_debugging"]
---

# Build and serve frontend assets

Keep source files such as SCSS in `priv/lib-src` and compiled assets in `priv/lib`. Use an app-level Makefile that delegates asset work to the source directory's Makefile.

Use `tag#lib` to include library files using Zotonic's asset handling. Follow the existing base template's placement of styles and scripts so dependencies load in the expected order.

After changing a source file, check the asset build output and the browser's network response. If the source changed but the served file did not, inspect the build step before flushing caches. Use [Reload the browser after source changes](../08-development-tools/live-reload.md) during development and [A saved change does not appear](../11-troubleshooting/changes-not-loaded.md) when changes remain invisible.

For a first stylesheet, create `priv/lib/css/garden.css` with a visible rule for a class in your page, then include it through `{% lib "css/garden.css" %}` in the site's head block. Check the loaded response in the browser. A plain CSS file needs no SCSS compiler; when you introduce generated CSS, document the exact Makefile target beside its source.

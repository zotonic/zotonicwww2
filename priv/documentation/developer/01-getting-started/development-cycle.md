---
name: "developer_development_cycle"
title: "Understand the development workflow"
summary: "Work in small, verifiable steps: change source, compile or reload, reproduce the request, and inspect the result."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 7
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging", "site_management"]
---

# Understand the development workflow

Work in small, verifiable steps: change source, compile or reload, reproduce the request, and inspect the result.

Templates and dispatch files are indexed per site. Erlang modules are loaded into the shared node. Database changes, module activation, and schema migrations have their own lifecycles; saving a file does not necessarily perform them.

After each change, check the layer involved. A successful Erlang compilation does not prove a URL matches the intended controller. A rendered template does not prove an anonymous visitor has permission to see its data.

Use a named example resource and a repeatable URL while developing. Keep test content separate from imported or production content. Finish with the relevant automated checks and a browser check when behavior is visible.

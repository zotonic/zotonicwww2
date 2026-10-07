---
name: "developer_automatic_rebuild"
title: "Automatic recompilation and file watching"
summary: "Zotonic's file handler watches source changes when a supported watcher is available. Erlang, templates, dispatch files, and assets have different handlers."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 8
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Automatic recompilation and file watching

Zotonic's file handler watches source changes when a supported watcher is available. Erlang, templates, dispatch files, and assets have different handlers.

1. Start a local development node and inspect watcher startup messages.
2. Change one known template and save it.
3. Check the terminal for the expected compile or index activity.
4. Reload the matching page.
5. Repeat with one Erlang change if you need to verify compilation and loading.

If nothing happens, check the watcher installation, watched path, and whether the application is known to Zotonic. An SCSS source can also depend on a project's Makefile rather than a generic file-extension handler.

Browser live reload is separate from server recompilation. See [Reload the browser after source changes](../08-development-tools/live-reload.md) and [A saved change does not appear](../11-troubleshooting/changes-not-loaded.md).

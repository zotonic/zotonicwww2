---
name: "developer_live_reload"
title: "Reload the browser after source changes"
summary: "Open System → Development and enable live reload. The page also enables separate CSS and JavaScript files when needed."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 3
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging"]
---

# Reload the browser after source changes

Open **System → Development** and enable live reload. The page also enables separate CSS and JavaScript files when needed.

Save a CSS change and confirm that the style updates. Save a template or JavaScript change and confirm that the page reloads. Finish unsaved form work before making a change that triggers a full reload.

Live reload depends on source changes reaching Zotonic's file handling and asset build. If nothing happens, check whether the compiled file changed and whether the browser connection is working. A reload cannot repair a failed compile.

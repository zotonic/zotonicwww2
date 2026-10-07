---
name: "developer_module_missing"
title: "A module is missing from the admin"
summary: "Check that the module is an Erlang application in a discovered project directory. Confirm the application name, .app.src, main mod_... module, and successful compilation."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 2
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "module_management"]
---

# A module is missing from the admin

Check that the module is an Erlang application in a discovered project directory. Confirm the application name, `.app.src`, main `mod_...` module, and successful compilation.

Then refresh discovery using the normal update workflow. Look for a compilation or dependency error in the logs. A directory containing templates alone is not necessarily a discoverable module application.

Once the module appears, activate it for the intended site and check its state. Discovery and activation are separate steps.

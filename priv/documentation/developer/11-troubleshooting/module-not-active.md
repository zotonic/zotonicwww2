---
name: "developer_module_not_active"
title: "A module is discovered but not active"
summary: "Open module management for the correct site. Check whether the module is disabled, waiting for a dependency, or failing during startup."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 3
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "module_management"]
---

# A module is discovered but not active

Open module management for the correct site. Check whether the module is disabled, waiting for a dependency, or failing during startup.

Read the activation error and inspect the declared dependencies and initialization callback. A successful Erlang compile does not prove that the module can start with this site's configuration and database.

Correct the cause, activate the module again, and verify one of its observable contributions, such as a template or observer registration.

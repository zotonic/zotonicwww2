---
name: "developer_site_not_starting"
title: "A site does not start"
summary: "Run bin/zotonic status to distinguish an unavailable node from a stopped or failing site. Inspect the relevant startup log before repeatedly restarting it."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 1
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "site_management", "configuration"]
---

# A site does not start

Run `bin/zotonic status` to distinguish an unavailable node from a stopped or failing site. Inspect the relevant startup log before repeatedly restarting it.

For a site failure, check configuration parsing, database access, required applications, and module initialization. Use `siteconfigfiles` to confirm which configuration files were selected. Do not paste credentials from configuration output into an issue.

Fix the first reported cause, start the site again, and verify an HTTP response. Later errors may be consequences of the first startup failure.

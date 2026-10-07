---
name: "admin_startup_failures"
title: "Diagnose a site that will not start"
summary: "Separate node, site, database, and application failures."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 3
required_modules: []
zotonic_keywords: ["troubleshooting", "operator", "site_management", "database", "configuration"]
---

# Diagnose a site that will not start

**Access needed:** Server access.

1. Run `bin/zotonic status` in the intended checkout and environment.
2. If the node is unavailable, inspect the service manager's state and startup logs. Check configuration syntax, Erlang version, port conflicts, and filesystem permissions.
3. If the node runs but one site fails, inspect that site's first startup error and enabled module dependencies.
4. Check its database connection from the running environment and verify the intended schema.
5. Compare code and configuration with the last working release. Correct one identified cause at a time.
6. Start or restart through the normal service procedure and check a real site page afterwards.

Do not repeatedly restart while a schema upgrade is still running. Preserve the original error and identify whether the operation is progressing, blocked, or failed. Avoid stopping the entire node to fix one site without considering other hosted sites.

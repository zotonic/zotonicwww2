---
name: "admin_maintenance_window"
title: "Run a planned maintenance window"
summary: "Coordinate changes and verify service before reopening access."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_maintenance"
order: 3
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "reliability"]
---

# Run a planned maintenance window

**Access needed:** Site operator and deployment owner.

1. Tell affected editors and service owners what will be unavailable and when.
2. Stop or pause writes, scheduled jobs, and integrations according to the installation's maintenance procedure.
3. Check the latest backup and save the current code/configuration revision.
4. Apply the planned changes and keep a short record of commands and outcomes without secrets.
5. Verify essential user workflows, queues, and background jobs before restoring normal operation.
6. Re-enable deliberately paused services and confirm that pending work is processed as intended.
7. Notify the affected team and record follow-up issues.

Do not assume that hiding a website page pauses background work or external callbacks. The maintenance mechanism depends on the proxy, site, and deployment arrangement. Use the agreed installation-specific procedure, and avoid stopping unrelated sites sharing the same Erlang node.

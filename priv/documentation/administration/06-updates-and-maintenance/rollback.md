---
name: "admin_rollback"
title: "Prepare and carry out recovery from a failed release"
summary: "Recover code and data to a compatible state."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_maintenance"
order: 2
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "reliability"]
---

# Prepare and carry out recovery from a failed release

**Access needed:** Deployment operator with database and backup access.

Before deployment, decide what failure triggers recovery and who makes that decision. Record the last working code, configuration, and backup together.

1. Stop new traffic or writes as required by the recovery plan and preserve failure logs.
2. Determine whether the release changed the database schema or external data.
3. If the previous code remains compatible, deploy its recorded revision and configuration through the normal service process.
4. If data must also be restored, use the approved restore procedure and account for work created since the backup.
5. Check site state, public pages, login, media, and the operation that failed.
6. Reopen traffic only after those checks, then document the recovery point and any lost or deferred work.

Do not blindly check out older code over an upgraded database. A restore can lose newer submissions, edits, and uploads; agree how those will be captured or reconciled. Rehearse the plan in acceptance before the release window.

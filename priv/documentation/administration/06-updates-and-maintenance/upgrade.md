---
name: "admin_upgrade"
title: "Plan and apply an upgrade"
summary: "Test version-specific changes and establish a recovery point."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_maintenance"
order: 1
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "reliability"]
---

# Plan and apply an upgrade

**Access needed:** Deployment and server access.

1. Read the release and upgrade notes for both Zotonic and changed site modules.
2. Record the current code revision and configuration. Identify schema migrations and changes to dependencies or external programs.
3. Build the target revision in acceptance using Erlang/OTP 28 for this guide's supported path.
4. Test startup with a representative data copy, then exercise login, editing, media processing, mail, and important integrations.
5. Confirm a recoverable pre-upgrade backup and a recovery plan for schema changes.
6. Schedule and deploy the tested revision using the release procedure.
7. Check logs and user workflows after deployment, then record the deployed revision.

Do not combine unrelated configuration and dependency changes merely because the site is already being updated. A restart that succeeds does not prove background migrations or processing jobs completed. Check those before declaring the upgrade finished.

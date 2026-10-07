---
name: "admin_deploy_release"
title: "Deploy and verify a release"
summary: "Apply a tested revision and check the site from a visitor’s perspective."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_deployment"
order: 3
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "site_management", "configuration", "reliability"]
---

# Deploy and verify a release

**Access needed:** Deployment and server access.

1. Record the release revision, configuration changes, dependencies, and database/schema changes.
2. Build and test that same revision in acceptance, including startup and upgrades.
3. Verify a recent recoverable backup and agree the recovery decision and responsible operator.
4. Schedule any interruption and follow the installation's service procedure. Apply code and configuration in the tested order.
5. Check `bin/zotonic status`, startup logs, and the intended site and module states.
6. Test a public page, login, a representative content operation, a media file, and any affected integration.
7. Record the outcome and monitor errors after reopening normal traffic.

A responding Erlang node does not prove a site is ready. If the release fails its checks, use the prepared recovery plan. Restoring older code alone may be incompatible with an upgraded database.

Use the deployment method selected for this installation; do not replace an existing service arrangement with an ad-hoc second Zotonic process.

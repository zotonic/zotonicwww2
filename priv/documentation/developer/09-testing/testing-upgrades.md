---
name: "developer_testing_upgrades"
title: "Verify an upgrade before release"
summary: "Prepare an isolated copy of the previously released application and database. Add representative edited content, then apply the new code and schema changes."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 5
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "migrate", "validate", "reliability"]
---

# Verify an upgrade before release

Prepare an isolated copy of the previously released application and database. Add representative edited content, then apply the new code and schema changes.

Check startup logs, module state, and a few operations that use the changed data. Repeat the deployment procedure so you can identify steps that depend on a one-time manual action.

Test recovery with the backup you intend to rely on. A rollback of source files alone may not undo a database migration. Record any required migration ordering in the release instructions.

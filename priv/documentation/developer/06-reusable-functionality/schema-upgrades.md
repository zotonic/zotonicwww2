---
name: "developer_schema_upgrades"
title: "Upgrade a module's data schema"
summary: "Version schema changes through the module's installation and upgrade mechanism. Inspect a module using -mod_schema and manage_schema/2 before adding your own versioned step."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 7
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "database", "migrate", "module"]
---

# Upgrade a module's data schema

Version schema changes through the module's installation and upgrade mechanism. Inspect a module using `-mod_schema` and `manage_schema/2` before adding your own versioned step.

Make each step safe for the state it may encounter after a partial attempt. Separate database structure changes from long-running data conversion when a single startup transaction would be too costly.

Test a fresh install and an upgrade from the previously released schema. Keep a database backup for the upgrade test and verify application behavior after migration, not just the table definitions.

If version 1 installed the Garden fixture, change the declaration to `-mod_schema(2).` for the next release. Keep the fresh-install clause and add a `manage_schema({upgrade, 2}, Context)` clause for the version-1-to-2 change. Erlang function clauses are separated with semicolons. Do not return success for an upgrade you have not implemented.

The manager calls upgrades in sequence, within a database transaction per schema step, then applies any returned datamodel and records the version. A code reload alone is not an upgrade test: run the module activation/startup lifecycle on the test site. Use `manage_data/2` for a deliberate follow-up after schema work, or queue longer conversions. Compare fresh-install and upgraded data before release.

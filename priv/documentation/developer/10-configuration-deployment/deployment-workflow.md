---
name: "developer_deployment_workflow"
title: "Deploy a repeatable release"
summary: "Record the code revision, build steps, configuration changes, and schema changes for the release. Build and test the same revision in an acceptance environment first."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 5
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["how_to_guide", "backend_developer", "configuration", "site_management"]
---

# Deploy a repeatable release

Record the code revision, build steps, configuration changes, and schema changes for the release. Build and test the same revision in an acceptance environment first.

Before deployment, verify that the required database and file backups can be recovered. Apply code and configuration in the planned order and monitor startup and upgrade logs. Confirm the intended sites and modules are running.

Finish with an HTTP check and one representative user workflow. A responding Erlang node is only one part of readiness. Keep a recovery plan that accounts for both code and database changes.

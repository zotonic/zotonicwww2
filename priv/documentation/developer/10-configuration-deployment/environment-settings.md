---
name: "developer_environment_settings"
title: "Keep development and production settings distinct"
summary: "Keep development and production settings distinct."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 3
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["how_to_guide", "backend_developer", "configuration", "site_management"]
---

# Keep development and production settings distinct

Set the site's environment intentionally and inspect the resolved value before relying on environment-specific tools. Development behavior can include trace controls, extra browser output, and asset settings that are unsuitable for normal visitor traffic.

Keep credentials outside shared documentation and committed examples. Give local installations their own database and hostname so an ordinary test cannot target the production site by accident.

Exercise the release configuration in an acceptance environment before deploying it. Compare the resolved settings, enabled modules, and storage paths when behavior differs between environments.

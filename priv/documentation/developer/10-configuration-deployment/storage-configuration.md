---
name: "developer_storage_configuration"
title: "Configure persistent file storage"
summary: "Identify where the site's archive files, generated previews, backups, and security files are stored. These have different retention and recovery needs."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_configuration_deployment"
order: 4
required_modules: []
source_paths: ["apps/zotonic_launcher/src", "apps/zotonic_core/src/support/z_config.erl", "apps/zotonic_mod_backup", "apps/zotonic_mod_filestore"]
zotonic_keywords: ["how_to_guide", "backend_developer", "file_storage", "file_store"]
---

# Configure persistent file storage

Identify where the site's archive files, generated previews, backups, and security files are stored. These have different retention and recovery needs.

Keep persistent runtime data outside directories that a code deployment replaces. If using `module#mod_filestore`, verify both the external storage configuration and retrieval behavior from the running site.

Upload a small test file, render a preview, and verify access after the relevant process restart. A successful database connection does not prove that media storage is working.

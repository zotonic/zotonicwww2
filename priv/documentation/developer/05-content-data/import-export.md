---
name: "developer_import_export"
title: "Move content between sites"
summary: "Export structured resource data and record how source resources map to destination resources. Match stable names or URIs deliberately; do not assume that numeric IDs match across sites."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 11
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "import_and_migration", "export_and_syndication"]
---

# Move content between sites

Export structured resource data and record how source resources map to destination resources. Match stable names or URIs deliberately; do not assume that numeric IDs match across sites.

Import media and resources before resolving relationships that depend on their destination IDs. Preserve ordered edges after all referenced resources exist. Keep an import report with created, updated, skipped, and failed items so the operation can be resumed safely.

Use the destination site's normal APIs and authorization. Test a small unpublished batch before importing a whole collection. See [Use stable resource names](resource-identifiers.md), [Connect resources and preserve order](content-edges.md), and [Verify backups by restoring them](../10-configuration-deployment/backup-recovery.md).

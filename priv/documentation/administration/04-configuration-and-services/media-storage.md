---
name: "admin_media_storage"
title: "Configure and check persistent media storage"
summary: "Keep original uploads accessible across deployments and restarts."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 5
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "file_storage", "media_management", "file_store"]
---

# Configure and check persistent media storage

**Access needed:** Server and storage access; module configuration permission.

Identify where originals, previews, site data, backups, and security files are stored. Treat originals as persistent data; a code checkout or temporary container filesystem is not a durable archive.

1. Confirm that the service account can write the intended local directories or access the configured external store.
2. For an external file store, configure the provider and credentials through the site's supported module settings.
3. Upload a small test image and another representative file, then verify both their original downloads and generated previews.
4. Restart the service in acceptance and repeat the checks.
5. Include the original-file store in backup and recovery planning, with an independent copy or provider recovery mechanism.

When moving an existing store, retain the old files until migration and retrieval have been verified. A database backup does not recreate original files missing from an external object store.

Use `module#mod_filestore` for detailed external-storage behaviour. Its activation can also change what the backup module includes.

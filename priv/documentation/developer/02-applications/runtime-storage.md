---
name: "developer_runtime_storage"
title: "Runtime data and uploaded files"
summary: "Keep runtime data separate from the application's source and static assets."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 11
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["how_to_guide", "backend_developer", "file_storage", "file_store"]
---

# Runtime data and uploaded files

Keep runtime data separate from the application's source and static assets.

Site media, generated previews, backups, logs, and caches can live outside the checkout. Their locations depend on the running installation's configuration. Use Zotonic's storage and path APIs rather than assuming a relative directory beside a template.

For media, work through `model#media` and related storage facilities so metadata, preview generation, access checks, and storage backends remain coordinated. Never replace a file behind the model's back as a normal upload workflow.

When deploying, preserve the site's durable files as well as its database. Rebuilding `_build` is not a backup or migration strategy for uploaded content.

---
name: "developer_media_data"
title: "Work with uploaded media"
summary: "A media resource has ordinary content properties and information about its associated file. Use model#media to inspect and manipulate media through Zotonic's normal processing path."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 7
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "media_management", "media_resource"]
---

# Work with uploaded media

A media resource has ordinary content properties and information about its associated file. Use `model#media` to inspect and manipulate media through Zotonic's normal processing path.

Keep the archive file and generated previews separate in your reasoning. A missing preview can be a processing problem even when the original upload is present. Check the file's media type, processing status, and storage configuration before replacing the resource.

Use media APIs for replacement and deletion so derived files and notifications remain consistent. Do not construct archive filenames from titles. See [Display images with mediaclasses](../04-templates/template-media.md), [Configure persistent file storage](../10-configuration-deployment/storage-configuration.md), and [An uploaded image does not display](../11-troubleshooting/missing-media.md).

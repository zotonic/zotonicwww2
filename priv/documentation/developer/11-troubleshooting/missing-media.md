---
name: "developer_missing_media"
title: "An uploaded image does not display"
summary: "Check whether the resource has an original file and whether its media information describes that file correctly. Then inspect the preview request in the browser network panel."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_troubleshooting"
order: 7
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_mod_development", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["troubleshooting", "backend_developer", "media_management", "file_storage"]
---

# An uploaded image does not display

Check whether the resource has an original file and whether its media information describes that file correctly. Then inspect the preview request in the browser network panel.

If the original exists but a preview fails, check processing logs, the requested image transformation, and file permissions or external storage access. If both fail, investigate the archive file and storage configuration first.

After fixing the cause, request the image again and verify the result at the size used by the page. Avoid deleting and recreating the resource before understanding the failure.

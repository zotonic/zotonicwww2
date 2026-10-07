---
name: "admin_media_failures"
title: "Diagnose failed uploads or missing media"
summary: "Locate the failure in permission, storage, or processing."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_monitoring"
order: 5
required_modules: []
zotonic_keywords: ["troubleshooting", "operator", "media_management", "file_storage", "logging_and_monitoring"]
---

# Diagnose failed uploads or missing media

**Access needed:** Site administrator; server/storage access for processing failures.

1. Record the media resource, upload time, file size and actual type, and affected user group.
2. If the upload was rejected, check the group policy and any proxy/server request limit.
3. If uploaded but not processed, inspect its processing status and logs. Check external tools such as ImageMagick or FFmpeg in the service account's environment.
4. For a missing original, check the configured local or external store and its access credentials.
5. For a missing preview, verify the original first, then investigate preview generation.
6. Correct the cause and retry one identified item through the supported interface. Check both download and preview.

Avoid repeatedly uploading the same large file while a job is still pending. A successful metadata record does not prove the binary reached durable storage. Escalate suspected loss of original files to the recovery owner before deleting records or clearing storage.

---
name: "admin_backup_plan"
title: "Define what a complete backup includes"
summary: "Choose recovery coverage, retention, and an owner."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_backups"
order: 1
required_modules: []
zotonic_keywords: ["explanation", "operator", "backup_and_restore", "reliability"]
---

# Define what a complete backup includes

**Access needed:** Site operator with database, storage, and configuration owners.

List what must be recovered: database content, original media, site code revision, configuration, credentials/security files, and external service data. Agree how much recent work may be lost and how long recovery may take.

1. Identify which mechanism backs up each item and where the copy is stored.
2. Check whether the site's backup module includes files or relies on external file-store protection.
3. Keep a recovery copy outside the failure boundary of the live installation.
4. Set retention and access permissions, including how encryption keys can be recovered.
5. Assign someone to check completion and storage capacity.
6. Schedule a restore rehearsal and record the backup age and recovery time achieved.

The backup module's rotating daily files are not an indefinite history. A database-only backup is useful but is not a complete site backup. Do not count a backup as usable until its required files and keys can actually be restored.

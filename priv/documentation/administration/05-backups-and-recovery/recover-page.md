---
name: "admin_recover_page"
title: "Recover one page without restoring the whole site"
summary: "Use revisions or deleted-page recovery for an editorial mistake."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_backups"
order: 4
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "backup_and_restore", "resource", "editorial_workflow"]
---

# Recover one page without restoring the whole site

**Access needed:** Editor or administrator with access to revisions and recovery.

First identify whether the page was changed, unpublished, disconnected from navigation, or deleted. Those problems have different fixes.

1. Find the page by title or known identifier and check its publication and connections.
2. If the body or fields changed, inspect the available revisions and compare them before restoring one.
3. If deleted, use the site's deleted-page recovery interface when enabled and inspect the candidate carefully.
4. Restore only the intended page or revision.
5. Check publication, access, media, and navigation connections afterwards; restoring text is not proof that every relationship has the desired state.
6. Verify the public result and tell the affected editors what was recovered.

A whole-database restore can discard everyone else's newer work. Use it only when page-level recovery cannot meet the recovery requirement and the operator has planned the wider impact. Available revision history depends on the site's retention and backup configuration.

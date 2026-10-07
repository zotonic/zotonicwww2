---
name: "admin_admin_scopes"
title: "Choose the site admin or the status site"
summary: "Use the right management surface and account."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_settings"
order: 1
required_modules: []
zotonic_keywords: ["explanation", "site_administrator", "site_management", "authorization_and_access_control"]
---

# Choose the site admin or the status site

**Access needed:** Site administrator; installation administrator for the status site.

A site's `/admin` manages that site's content, users, and enabled features. The status site manages sites on the running Zotonic installation. The server shell manages processes, files, and configuration. These permissions are separate.

1. Confirm the hostname and site you intend to change.
2. Use that site's `/admin` for browser tasks in this guide.
3. For installation management, open the status site's configured address and use its `wwwadmin` credentials. An individual site's `admin` password may be different.
4. If you have server access, run `bin/zotonic status` from the correct checkout to check the node and site states.
5. Recheck the hostname before saving changes, especially when several environments look alike.

Do not copy status-site passwords or complete configuration output into tickets. Ask the installation owner for access rather than trying a production credential on an unrelated environment.

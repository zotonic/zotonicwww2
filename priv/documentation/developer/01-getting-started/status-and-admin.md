---
name: "developer_status_and_admin"
title: "Find the status site and your site's admin"
summary: "The status site manages the running Zotonic installation. A site's /admin manages that site's content and enabled modules. They have different scopes and can use different credentials."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 4
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "site_management"]
---

# Find the status site and your site's admin

The status site manages the running Zotonic installation. A site's `/admin` manages that site's content and enabled modules. They have different scopes and can use different credentials.

Start with `bin/zotonic status` to identify your site and its state. Use `bin/zotonic open garden` to open its configured URL, then follow the site's login or admin route.

In the admin, locate module management and **System → Development**. The latter appears when `mod_development` is enabled and your account has access.

If the wrong site opens, inspect the hostname, aliases, and port rather than changing content. Several sites can run inside the same node. See [Site configuration and enabled modules](../02-applications/site-configuration.md) and [Enable the Development module](../08-development-tools/enable-development.md).

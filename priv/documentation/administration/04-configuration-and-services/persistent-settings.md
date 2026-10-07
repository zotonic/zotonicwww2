---
name: "admin_persistent_settings"
title: "Change persistent configuration"
summary: "Choose the right configuration layer and verify the effective value."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 1
required_modules: []
zotonic_keywords: ["how_to_guide", "operator", "configuration", "configure"]
---

# Change persistent configuration

**Access needed:** Site configuration permission or server configuration access, depending on the setting.

1. Find the setting's definition and determine whether it belongs to the global installation, the site's configuration file, or a module's database settings.
2. Record the current effective value and which source supplies it. Check environment-variable or deployment overrides.
3. Change the owning source in acceptance and apply the documented reload or restart for that setting.
4. Test the affected behaviour and repeat after a restart to confirm persistence.
5. Deploy the same change through the site's normal configuration process and record the result.

`bin/zotonic setconfig` changes runtime configuration; it does not save a configuration file. Do not use a successful runtime experiment as proof that a restart will preserve the setting.

Configuration output can include passwords and tokens. Inspect it locally and share only redacted relevant keys. Store secrets in the installation's protected configuration mechanism, not documentation or public resource text.

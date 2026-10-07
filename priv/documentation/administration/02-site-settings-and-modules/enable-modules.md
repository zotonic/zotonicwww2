---
name: "admin_enable_modules"
title: "Enable or disable a site feature"
summary: "Change module activation and verify the affected workflow."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_settings"
order: 2
required_modules: []
zotonic_keywords: ["how_to_guide", "site_administrator", "module_management", "configure"]
---

# Enable or disable a site feature

**Access needed:** Site administrator with module-management permission.

::: aside
Module activation and adding an Erlang application to the deployment are different operations.
:::

Identify the module that provides the feature and read its setup requirements. A module must be installed on the server before it can be enabled for a site.

1. Open **System → Modules** in the intended site's admin.
2. Find the module and check its dependencies and current state.
3. Enable it and wait for activation to finish. Read any error instead of repeatedly toggling it.
4. Open the feature's settings and complete the required configuration.
5. Test the feature with the intended user role, including a public check if visitors use it.
6. Record the change so the test and production environments can be configured consistently.

Before disabling a module, check which pages, templates, scheduled jobs, or other modules depend on it. Disabling a feature can make existing content unavailable; it does not necessarily remove its stored data. Test the effect in acceptance first.

If the module is missing, ask the developer to install and build it.


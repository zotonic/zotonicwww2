---
name: "developer_create_module"
title: "Create a reusable module"
summary: "Create apps_user/zotonic_mod_garden with the following files. This example adds a discoverable module without starting an extra process."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 1
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "module", "erlang_otp"]
---

# Create a reusable module

Create `apps_user/zotonic_mod_garden` with the following files. This example adds a discoverable module without starting an extra process.

`rebar.config`:

```erlang
{erl_opts, [debug_info]}.
```

`src/zotonic_mod_garden.app.src`:

```erlang
{application, zotonic_mod_garden, [
    {description, "Community garden features"},
    {vsn, "0.1.0"},
    {registered, []},
    {applications, [kernel, stdlib, zotonic_core]},
    {env, []},
    {modules, []}
]}.
```

`src/mod_garden.erl`:

```erlang
-module(mod_garden).
-mod_title("Garden").
-mod_description("Community garden features.").
-mod_prio(500).
```

Run `make` from the Zotonic workspace root. On the running development node, use `bin/zotonic update` to refresh discovery. Open module management in the site's admin and activate Garden.

Check that the module is active. Add the model from [Expose data through a model](model-api.md) for a first data lookup, and put templates in `priv/templates`. Keep installation hooks and observers in the main module; move domain logic into models or support modules.

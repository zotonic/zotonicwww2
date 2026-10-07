---
name: "developer_module_priority"
title: "Dependencies, capabilities, and module priority"
summary: "The main site or module can declare:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_applications"
order: 8
required_modules: []
source_paths: ["rebar.config", "apps/zotonic_core/src/support/z_module_manager.erl", "apps/zotonic_core/src/support/z_module_indexer.erl", "apps/zotonic_mod_zotonic_site_management/priv/skel"]
zotonic_keywords: ["explanation", "backend_developer", "module", "template"]
---

# Dependencies, capabilities, and module priority

The main site or module can declare:

```erlang
-mod_depends([mod_base]).
-mod_prio(500).
```

Dependencies express requirements for activation. Modules can also declare provided capabilities with `-mod_provides`; depend on an established capability when your feature requires that service rather than one particular implementation.

Priority determines ordering when modules supply competing resources such as templates. Lower numerical priorities take precedence. Sites commonly use a low priority so their templates override generic module templates.

Do not use priority to hide a missing dependency or duplicate Erlang module name. Check which modules are active, inspect the selected template, and make one intentional override at a time.

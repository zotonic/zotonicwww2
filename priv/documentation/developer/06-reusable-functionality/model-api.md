---
name: "developer_model_api"
title: "Expose data through a model"
summary: "Create src/models/m_garden.erl in the Garden module. This complete example exposes a public greeting:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 3
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "model", "api_and_integration"]
---

# Expose data through a model

Create `src/models/m_garden.erl` in the Garden module. This complete example exposes a public greeting:

```erlang
-module(m_garden).
-behaviour(zotonic_model).
-export([m_get/3]).

-spec m_get(Path, Msg, Context) -> Result
    when
        Path :: list(),
        Msg :: zotonic_model:opt_msg(),
        Context :: z:context(),
        Result :: zotonic_model:return().
m_get([<<"greeting">> | Rest], _Msg, _Context) ->
    {ok, {<<"Welcome to the garden">>, Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.
```

Compile the application and confirm that Garden is active on the site. Read the value in a template:

```django
<p>{{ m.garden.greeting|escape }}</p>
```

Expect the greeting in the rendered page. The callback matches binary path segments and returns the unused path for further lookup. Add explicit permission checks before returning private data, and validate input before an operation changes anything.

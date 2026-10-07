---
name: "developer_template_extensions"
title: "Add a filter or rendering component"
summary: "Use a filter for a small transformation of an input value. Keep missing and unexpected values predictable, and escape output at the point where it becomes HTML."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 6
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "template", "template_filter", "scomp"]
---

# Add a filter or rendering component

Use a filter for a small transformation of an input value. Keep missing and unexpected values predictable, and escape output at the point where it becomes HTML.

Use a scomp when rendering needs arguments, context, or more substantial server work. Follow an existing scomp's callback contract and declare caching behavior deliberately.

Place the implementation in the application's `src/filters` or `src/scomps` directory and follow Zotonic naming conventions. Compile and confirm discovery before changing templates to depend on it.

A complete filter example in `src/filters/filter_garden_label.erl`:

```erlang
-module(filter_garden_label).
-export([garden_label/2]).

-spec garden_label(Value, Context) -> binary()
    when Value :: term(), Context :: z:context().
garden_label(<<"open">>, _Context) -> <<"Open to visitors">>;
garden_label(_Value, _Context) -> <<"Check visiting times">>.
```

Compile it and render `{{ "open"|garden_label|escape }}`. Expect “Open to visitors”; a missing value should produce the fallback. These fixed English labels illustrate the callback only: use translation support for real interface labels.

For a scomp, use the current `zotonic_scomp` behaviour and its `render/3` callback, not the old `gen_scomp` behaviour. Start with the source of `scomp#button` for a maintained example that queues browser actions.

---
name: "developer_postback_handlers"
title: "Handle a postback on the server"
summary: "Set the wire's delegate to the module that handles the event. Implement its exported event/2 callback with a pattern matching the expected event record."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 4
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "wire_action", "forms", "erlang_otp"]
---

# Handle a postback on the server

Use the active Garden module as the delegate. Add this header, export, and callback to `mod_garden.erl`, merging with declarations and event clauses already present:

```erlang
-include_lib("zotonic_core/include/zotonic.hrl").
-export([event/2]).

-spec event(Event, Context) -> z:context()
    when Event :: #postback{}, Context :: z:context().
event(#postback{message = {garden_hello, []}}, Context) ->
    z_render:growl(?__("Welcome to the garden", Context), Context).
```

In a page with the standard JavaScript includes, `mod_wires`, and a final `{% script %}`:

```django
<button id="garden-hello" type="button">{_ Say hello _}</button>
{% wire id="garden-hello" postback={garden_hello} delegate=`mod_garden` %}
```

Compile and load the module, reload the page, and click the button. Expect the greeting notification. The returned context contains the queued browser response; returning the original context after another response call can lose that response.

This public example changes no stored data. For a write, read only supported binary query keys, validate input, and authorize with the supplied context before calling a model. A signed postback does not make arbitrary form values trustworthy. A form submit delivers `#submit{}`, not `#postback{}`.

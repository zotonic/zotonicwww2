---
name: "developer_observers"
title: "Respond to a notification"
summary: "Find the notification definition and inspect its callers before implementing an observer. The notifier operation determines whether observers collect results, stop at a first result, or fold a value."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 5
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "notification"]
---

# Respond to a notification

Find the notification definition and inspect its callers before implementing an observer. The notifier operation determines whether observers collect results, stop at a first result, or fold a value.

Add the appropriate exported `observe_...` callback to your active module. Keep it short and return the value required by that notification's contract. Move slow work out of synchronous request paths where possible.

Use the Development observer list to verify registration. Test with other modules enabled: an observer that works alone may interact with another observer through priority or return values.

For example, add the header, export, and callback below to `mod_garden.erl` (merge declarations with existing ones):

```erlang
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe_rsc_update_done/2]).

-spec observe_rsc_update_done(Event, Context) -> ok
    when Event :: #rsc_update_done{}, Context :: z:context().
observe_rsc_update_done(#rsc_update_done{action = Action, id = Id}, _Context) ->
    ?LOG_INFO(#{text => <<"Garden observed a resource change">>,
                in => mod_garden, action => Action, id => Id}),
    ok.
```

Compile, refresh module discovery, check the observer list, then save a practice page. Expect a log entry with its ID. This notification runs after persistence and ignores the observer's return value. Do not update the same resource unconditionally from this callback: that can trigger the observer again. `notification#rsc_update_done` and `notification#rsc_update` have different contracts.

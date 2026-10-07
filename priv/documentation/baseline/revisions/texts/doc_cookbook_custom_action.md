# Create a reusable browser action

Use an active module named `mod_garden`. Create `src/actions/action_garden_welcome.erl`:

```erlang
-module(action_garden_welcome).
-export([render_action/4]).

render_action(TriggerId, TargetId, _Args, Context) ->
    z_render:render_actions(TriggerId, TargetId,
        [{growl, [{text, <<"Welcome to the garden">>}]}], Context).
```

Compile the application, refresh module discovery, and add this to a page using the normal Zotonic browser scripts:

```django
{% button text="Welcome" action={welcome} %}
```

Click the button. Expect the growl message. `render_action/4` returns JavaScript together with the updated context. Delegating to a built-in action preserves its encoding and wiring behaviour.

This action only displays constant text. To change data, send a postback to an exported `event/2` handler, validate input and check authorization there; a hidden button or signed postback is not permission to modify a resource.

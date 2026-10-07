# Show a growl message

Start from a working Zotonic page whose base template includes the site's standard browser scripts and script output. Do not copy an old `z.notice.js` path or load a second jQuery version.

```django
{% button text="Show message" action={growl text="Your changes were saved."} %}
```

Click the button and check that the message appears. This example demonstrates the action only; a real save handler should add a success message after persistence succeeds. From an Erlang event handler, return the context from `z_render:growl(<<"Your changes were saved.">>, Context)`.

If nothing happens, check the browser console, the site's base-template script includes, and whether `mod_wires` is active. Do not solve a missing script dependency by embedding an obsolete standalone plugin.

---
name: "developer_browser_events"
title: "Handle a browser event with a wire"
summary: "Use scomp#wire to connect a browser event to an action or a server postback. Give the target element a stable ID within the rendered page."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 3
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "javascript", "wire_action", "user_interface_and_interaction"]
---

# Handle a browser event with a wire

Use `scomp#wire` to connect a browser event to an action or a server postback. Give the target element a stable ID within the rendered page.

```django
<button id="show-help" type="button">{_ Show help _}</button>
<div id="garden-help" style="display:none">{_ Choose a sunny spot. _}</div>
{% wire id="show-help" action={show target="garden-help"} %}
```

Use a server postback when the action needs server data or writes content. A client-side action alone cannot authorize a write. Check the browser console and the server logs when an event appears to do nothing.

Use a base template with the standard JavaScript includes and a final `{% script %}`; enable `mod_wires` on the site. Put the example inside its content block. Click **Show help** and expect the hidden text to appear without navigation. If this partial can appear twice, use generated template IDs such as `#show_help` rather than repeating the literal ID.

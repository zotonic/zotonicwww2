---
name: "developer_collection_browser_apis"
title: "Browser interaction and APIs"
summary: "Connect browser actions to server-side behavior."
category: "collection"
language: "en"
is_published: true
parent: "developer_guide"
order: 7
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["explanation", "frontend_developer", "user_interface_and_interaction", "api_and_integration"]
---

# Browser interaction and APIs

Connect browser actions to server-side behavior.

Choose the task that matches what you need to do. The pages explain the relevant workflow and link to related guides and reference material. Examples use the local site `garden`; use your own site name and check the command’s scope before running it.

Start with the greeting model in **Expose data through a model**, then call it through browser messaging or HTTP. Use wires for declarative page actions and postbacks when an Erlang handler should respond to a form or button. These approaches can coexist on one page.

---
name: "developer_extension_points"
title: "Choose a model, controller, observer, or component"
summary: "Choose a model, controller, observer, or component."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_reusable_functionality"
order: 2
required_modules: []
source_paths: ["apps/zotonic_core/src/behaviours", "apps/zotonic_mod_base/src", "apps/zotonic_core/include/zotonic_notifications.hrl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "notification", "module"]
---

# Choose a model, controller, observer, or component

Choose the extension point from the caller's task. A model exposes site-aware data and operations. A controller handles an HTTP endpoint. An observer responds to a Zotonic notification. A filter transforms a template value; a scomp renders a template component.

Keep shared domain logic in support functions and call it from these boundaries. Avoid making a controller the only way another Erlang module can use a feature.

Look at an existing implementation with the same return contract before adding a callback. Similar names do not guarantee identical callback signatures.

---
name: "developer_dispatch_debugging"
title: "Trace a URL through dispatch"
summary: "Open the Development page's dispatch tools. Inspect the list of active rules, then trace the path that produces an unexpected result."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 7
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "dispatch_rule", "routing_and_redirects"]
---

# Trace a URL through dispatch

Open the Development page's dispatch tools. Inspect the list of active rules, then trace the path that produces an unexpected result.

Check the matched rule, controller, arguments, and any rewrite steps. Verify that you are testing the intended hostname and site. Compare the result with `bin/zotonic dispatch garden /path` when you need a terminal view.

If a resource has its own page path, inspect that resource's URL settings too. After changing a rule, refresh the dispatch information and make a real browser request.

---
name: "developer_template_trace"
title: "Trace templates rendered by a request"
summary: "Open the Development page's Live dependency graph of templates tool. Start a trace for your session, then load the page or path you want to inspect."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 4
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "template", "development_and_debugging"]
---

# Trace templates rendered by a request

Open the Development page's **Live dependency graph of templates** tool. Start a trace for your session, then load the page or path you want to inspect.

Read the resulting graph from the outer template toward its includes. Follow file locations to see which module supplied each template. Use a small request first so repeated includes do not obscure the result.

Stop the trace when you have captured the request. The option to trace all sessions collects more activity; use it only when a session-specific trace cannot reproduce the problem.

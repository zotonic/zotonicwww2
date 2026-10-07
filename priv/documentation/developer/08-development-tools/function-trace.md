---
name: "developer_function_trace"
title: "Trace a small number of Erlang calls"
summary: "Open the Development function tracing tool as the admin user. Enter a module, optionally a function, and a small limit on the number of calls."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 10
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Trace a small number of Erlang calls

Open the Development function tracing tool as the admin user. Enter a module, optionally a function, and a small limit on the number of calls.

Start the trace, reproduce the operation once, and inspect the output. Narrow the function selection if unrelated calls fill the limit. Arguments and results can contain application data, so keep trace output within the intended debugging context.

The tool depends on the global `function_tracing_enabled` setting. Check the configured environment and permissions if the link is unavailable; do not change production settings merely to make a local workflow match.

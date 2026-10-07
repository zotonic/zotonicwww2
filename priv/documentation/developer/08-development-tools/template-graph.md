---
name: "developer_template_graph"
title: "Inspect template dependencies"
summary: "Open Dependency graph of all available templates from the Development page. Use the graph to understand inheritance and include relationships before changing a shared template."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 5
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "template", "development_and_debugging"]
---

# Inspect template dependencies

Open **Dependency graph of all available templates** from the Development page. Use the graph to understand inheritance and include relationships before changing a shared template.

A static dependency graph shows possible relationships. It does not prove that a conditional include ran in a particular request. Use a live trace for that question.

Follow an edge to identify the caller of a partial, then check whether other pages share it. For dynamic template names, inspect the code that supplies the name as well. See [Trace templates rendered by a request](template-trace.md), [Reuse markup with includes and categories](../04-templates/template-components.md), and [Check template references](template-xref.md).

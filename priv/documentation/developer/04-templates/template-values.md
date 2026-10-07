---
name: "developer_template_values"
title: "Display values safely"
summary: "Resource properties read through m.rsc follow Zotonic's content handling. Query arguments and values returned by custom models do not automatically have the same guarantees."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 7
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "security", "html"]
---

# Display values safely

Resource properties read through `m.rsc` follow Zotonic's content handling. Query arguments and values returned by custom models do not automatically have the same guarantees.

Escape plain text from those sources:

```django
<p>{{ q.term|escape }}</p>
```

Do not mark arbitrary user input as safe HTML. Decide at the model or content boundary whether a field contains text, sanitized HTML, a URL, or another type. Keep that decision consistent across templates and API responses.

When a value renders unexpectedly, inspect its type and translation behavior before applying more filters. See `filter#escape`, [Expose data through a model](../06-reusable-functionality/model-api.md), and [Keep permission checks at the boundary](../05-content-data/access-control.md).

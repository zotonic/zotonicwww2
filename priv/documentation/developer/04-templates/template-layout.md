---
name: "developer_template_layout"
title: "Create a page layout"
summary: "Use an existing site base template as your starting point. Put shared document structure in the base and page-specific markup in blocks."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 3
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "render"]
---

# Create a page layout

Use an existing site base template as your starting point. Put shared document structure in the base and page-specific markup in blocks.

```django
{% extends "base.tpl" %}
{% block content %}
<main>
    <h1>{_ Our garden _}</h1>
    <p>{_ Find out what is growing this week. _}</p>
</main>
{% endblock %}
```

Check which blocks your chosen base provides before overriding one. Keep the standard head and body includes so active modules can contribute scripts, styles, metadata, and browser initialization. In a custom base, use `tag#all_include` with the standard include names instead of copying module-specific head fragments.

In a custom base, put `{% all include "_html_head.tpl" %}` inside `<head>`. Near the end of `<body>`, keep `{% all include "_html_body.tpl" %}`, the site's `_js_include.tpl`, and one final `{% script %}` in their established order. Wires collect browser code for that final script tag; without it a page can render correctly while its buttons do nothing.

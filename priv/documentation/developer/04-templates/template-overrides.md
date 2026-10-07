---
name: "developer_template_overrides"
title: "Override a module template"
summary: "Find the active template and its path relative to priv/templates. Create the same relative path in your site or another active module with a higher selection priority."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 5
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "render"]
---

# Override a module template

Find the active template and its path relative to `priv/templates`. Create the same relative path in your site or another active module with a higher selection priority.

Copy only the structure you need to change. When the original template offers blocks, extend it using the supported inheritance pattern rather than duplicating a large file. Check `tag#overrules` for extending the next matching template.

Load the affected page and inspect the selected template again. A successful compile does not prove that your override is used. Test neighboring categories and languages if the original template serves more than one page type.

For example, place this in the same relative template path as the template being overridden:

```django
{% overrules %}
{% block content %}
    {% inherit %}
    <p>{_ Contact us for more information. _}</p>
{% endblock %}
```

Use a block that exists in the original. `{% inherit %}` keeps its content; omit it only when replacing that entire block is intentional. This is especially important for admin panels, where copying a small fragment over a large panel can remove controls.

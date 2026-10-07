---
name: "developer_template_components"
title: "Reuse markup with includes and categories"
summary: "Extract repeated markup into a partial and pass its inputs explicitly."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 4
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "render"]
---

# Reuse markup with includes and categories

Extract repeated markup into a partial and pass its inputs explicitly.

```django
{% include "_garden_card.tpl" id=id %}
```

Inside the partial, read the resource using `m.rsc[id]`. Keep the partial focused on one component so another page can reuse it without inheriting unrelated page behavior.

Use a category include when the representation depends on the resource's category:

```django
{% catinclude "_garden_card.tpl" id %}
```

Provide a generic fallback and add category-specific variants when their markup differs. Use the template selection tools to check which variant wins for the resource. See `tag#include`, `tag#catinclude`, and [Find the template used by a page](template-selection.md).

For the example above, save the fallback as `priv/templates/_garden_card.tpl`. An event-specific version is `_garden_card.event.tpl`. A resource named `garden_open_day` can override both with `_garden_card.name.garden_open_day.tpl`. Test an event and an ordinary text resource: the latter should still use the fallback.

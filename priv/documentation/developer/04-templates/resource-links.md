---
name: "developer_resource_links"
title: "Link to resources and dispatch routes"
summary: "Use a resource's page URL when linking to content. This lets Zotonic account for its path and language."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 6
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "resource", "url"]
---

# Link to resources and dispatch routes

Use a resource's page URL when linking to content. This lets Zotonic account for its path and language.

```django
<a href="{{ m.rsc.page_home.page_url }}">{_ Home _}</a>
```

For an application endpoint, use a named dispatch route:

```django
<a href="{% url admin %}">{_ Admin _}</a>
```

Do not add separate normal content routes for each language. Configure the resource and language support instead. Avoid building internal URLs by joining strings: escaping, optional arguments, and language prefixes belong to the dispatcher.

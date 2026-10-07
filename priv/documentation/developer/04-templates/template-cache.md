---
name: "developer_template_cache"
title: "Cache a template fragment"
summary: "First measure the work performed by the fragment. Cache a fragment only when you know which inputs and data changes affect its output."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 10
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "cache", "performance"]
---

# Cache a template fragment

First measure the work performed by the fragment. Cache a fragment only when you know which inputs and data changes affect its output.

Check `tag#cache` for the supported duration, variation, and dependency arguments. A fragment containing a user's private information must not be shared between users through an incomplete cache key. Language and resource identity can also change the result.

Test a cache hit, then change the underlying content and check invalidation. During debugging, the Development page can disable template cache tags. Restore that setting before measuring normal page behavior. See [Distinguish stale cache data from stale code](../08-development-tools/cache-debugging.md) and [Choose checks for a change](../09-testing/testing-strategy.md).

For a public resource card, a starting point is:

```django
{% cache 60 garden_card vary=id vary=z_language if_anonymous %}
    <a href="{{ id.page_url }}">{{ id.title }}</a>
{% endcache %}
```

The resource ID varies the entry and is a cache dependency; language separates translations. This example caches only anonymous requests. If the fragment also reads connected resources, add the corresponding dependencies. Rename a shared cache block when its output contract changes.

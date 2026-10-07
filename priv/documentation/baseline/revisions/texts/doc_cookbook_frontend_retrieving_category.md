# Read a page category in a template

The page controller supplies `id`. Resolve its category directly:

```django
{% with id.category_id as category_id %}
    <p>{{ category_id.title }}</p>
{% endwith %}
```

Use `id.is_a.article` for an inherited category test, replacing `article` with your category's name. To select category-specific rendering, prefer `tag#catinclude` over a long chain of category conditions.

A page URL is not a reliable category identifier: custom paths, language prefixes and dispatch rules can change its shape. For a value from a request, resolve it through `m.rsc[value].id` and handle a missing resource before reading properties.

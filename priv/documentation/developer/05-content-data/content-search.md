---
name: "developer_content_search"
title: "Search for resources"
summary: "Use model#search and Zotonic's query language for ordinary content searches. Keep the query close to the page's actual task: filter by category, connection, or date, then choose an explicit sort order."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_content_data"
order: 6
required_modules: []
source_paths: ["apps/zotonic_core/src/models", "apps/zotonic_core/src/support/z_datamodel.erl", "apps/zotonic_core/src/support/z_search.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "search_and_discovery", "query"]
---

# Search for resources

Use `model#search` and Zotonic's query language for ordinary content searches. Keep the query close to the page's actual task: filter by category, connection, or date, then choose an explicit sort order.

Use pagination for lists that can grow. Test with no results, one result, and enough results to reach the next page. Run the same search as a visitor to check publication and visibility behavior.

Before writing SQL, inspect existing query options and named queries. A custom query should preserve the same access rules as the page that displays it. See [Add a database query when models are not enough](database-queries.md) and [Keep permission checks at the boundary](access-control.md).

For example, with `mod_search` enabled, show at most ten text resources:

```django
{% with m.search.query::%{cat: "text", sort: "-publication_start", pagelen: 10} as result %}
    <ul>
    {% for item in result %}
        <li><a href="{{ item.page_url }}">{{ item.title }}</a></li>
    {% empty %}
        <li>{_ No pages found. _}</li>
    {% endfor %}
    </ul>
{% endwith %}
```

This is a bounded preview, not a complete paginated archive. For an archive, pass the current page into the query and render `scomp#pager` with the result and your listing route. Test the second page; raising `pagelen` is not pagination. The query reference in `model#search` lists filters and result forms.

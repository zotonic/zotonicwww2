# Add a named resource search

First try a query resource or the built-in query search. For a new named search, add this callback to the active `mod_garden` module:

```erlang
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe_search_query/2]).

observe_search_query(#search_query{name = <<"garden_featured">>}, _Context) ->
    #search_sql{
        select = "r.id",
        from = "rsc r",
        where = "r.is_featured = $1",
        args = [true],
        order = "r.pivot_title, r.id",
        tables = [{rsc, "r"}]
    };
observe_search_query(_Query, _Context) ->
    undefined.
```

Merge the include and export into the existing module; do not duplicate its module declaration. Compile and refresh observer registration. Mark a practice resource as featured, then render:

```django
{% for result_id in m.search.garden_featured %}
    <p><a href="{{ result_id.page_url }}">{{ result_id.title }}</a></p>
{% empty %}
    <p>No featured pages are visible.</p>
{% endfor %}
```

Declaring the `rsc` table alias allows the search engine to add its normal visibility constraints. Test as a visitor and as an editor, including an unpublished or restricted resource. Use SQL parameters for values and fixed, reviewed SQL for identifiers. A search of unrelated custom tables does not automatically gain resource ACL checks.

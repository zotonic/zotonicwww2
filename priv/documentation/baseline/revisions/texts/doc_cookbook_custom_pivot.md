# Index a custom resource property

Use a custom pivot when a resource property must be filtered or sorted in SQL. Define the table in the active module's schema callback, and increase `-mod_schema(...)` when adding it to an already installed module:

```erlang
manage_schema(_Version, Context) ->
    ok = z_pivot_rsc:define_custom_pivot(garden,
        [{requestor, "text"}], Context),
    ok.
```

Merge this operation into existing schema handling; do not replace unrelated upgrades. Export `manage_schema/2` and `observe_custom_pivot/2`, and include `zotonic.hrl`:

```erlang
observe_custom_pivot(#custom_pivot{id = Id}, Context) ->
    {garden, [{requestor, m_rsc:p(Id, requestor, Context)}]}.
```

The table is `pivot_garden`. The current helper alters the table when column names differ; do not rely on the old claim that every definition change drops and recreates it. A type change with the same column names needs an explicit, reviewed database migration.

Compile and activate the module upgrade, then rebuild search indexes from the site's status tools. Wait for pivoting to complete. Save a practice resource with a `requestor` property and check both filtering and sorting:

```django
{% for result_id in m.search[{query filter=["pivot.garden.requestor", `=`, "Alice"] sort="pivot.garden.requestor"}] %}
    <p>{{ result_id.title }}</p>
{% endfor %}
```

Test as the intended user so visibility rules still apply. Defining a column alone does not populate existing resources.

# Create a custom template tag

In the active Garden module, create `src/scomps/scomp_garden_greeting.erl`:

```erlang
-module(scomp_garden_greeting).
-behaviour(zotonic_scomp).
-export([render/3, vary/2]).

vary(_Params, _Context) -> nocache.
render(Params, _Vars, _Context) ->
    Name = proplists:get_value(name, Params, <<"visitor">>),
    {ok, [<<"Hello, ">>, z_html:escape(z_convert:to_binary(Name))]}.
```

Compile and refresh module discovery. In a template:

```django
<p>{% greeting name="Alice" %}</p>
```

Expect “Hello, Alice”. Test a name containing `<` and `&` too: it must appear as text. `render/3` receives tag parameters, template variables and the current site context. `vary/2` declares caching behaviour; this example deliberately uses `nocache`. For simple transformations of one input value, use a filter instead.

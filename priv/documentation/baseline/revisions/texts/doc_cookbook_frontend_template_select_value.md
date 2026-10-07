# Update a preview from a selection

Use a wired change event to update part of a page. This example offers two known resources; create them with names `garden_rose` and `garden_tree` first.

```django
<label for="{{ #choice }}">{_ Choose a plant _}</label>
<select id="{{ #choice }}" name="plant">
    <option value="">{_ Choose… _}</option>
    <option value="garden_rose">{_ Rose _}</option>
    <option value="garden_tree">{_ Tree _}</option>
</select>
<div id="{{ #preview }}"></div>
{% wire id=#choice type="change"
        postback={plant_preview target=#preview} delegate="mod_garden" %}
```

In the active `mod_garden`, include `zotonic.hrl`, export `event/2`, and merge this clause into its existing handler:

```erlang
event(#postback{message = {plant_preview, Args}}, Context) ->
    Name = z_context:get_q(<<"triggervalue">>, Context),
    case lists:member(Name, [<<"garden_rose">>, <<"garden_tree">>]) of
        true ->
            Id = m_rsc:rid(Name, Context),
            case m_rsc:is_visible(Id, Context) of
                true ->
                    Target = proplists:get_value(target, Args),
                    z_render:update(Target, "_garden_plant_preview.tpl",
                                    [{id, Id}], Context);
                false -> z_render:growl_error(<<"This plant is unavailable.">>, Context)
            end;
        false ->
            z_render:growl_error(<<"Choose a plant from the list.">>, Context)
    end.
```

Create `priv/templates/_garden_plant_preview.tpl`:

```django
<h2>{{ id.title }}</h2>
{% if id.depiction %}{% image id.depiction width=300 %}{% endif %}
```

Compile, refresh module discovery, then change the selection. Expect the corresponding visible page's title and image. Test an empty value, a manually altered value and a restricted resource as a visitor. The signed postback carries the target; the submitted selection remains untrusted. For a large dynamic list, validate against its actual allowed resource set instead of this fixed list.

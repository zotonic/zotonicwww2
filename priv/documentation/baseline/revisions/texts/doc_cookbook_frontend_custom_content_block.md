# Add a custom content block

The block menu comes from `model#admin_blocks`. Register a callout type through its `admin_edit_blocks` notification. In the active site or module Erlang file, export `observe_admin_edit_blocks/3` and include `zotonic.hrl`:

```erlang
observe_admin_edit_blocks(_Event, Blocks, _Context) ->
    [{100, <<"Garden">>, [{callout, <<"Callout">>}]} | Blocks].
```

Create `priv/templates/blocks/_admin_edit_block_li_callout.tpl`:

```django
<label for="{{ #text }}">{_ Callout text _}</label>
<textarea id="{{ #text }}" name="blocks[].text"
          class="form-control">{{ blk.text|escape }}</textarea>
```

Create `priv/templates/blocks/_block_view_callout.tpl`:

```django
<aside class="callout">{{ blk.text|escape }}</aside>
```

Use the site's existing page-block rendering loop (`{% include "_blocks.tpl" %}` in the standard layout). Compile, refresh observer registration, then add a callout in admin, save, reopen and view the public page. Confirm the text survives saving and markup entered in the field remains plain text.

Use `blocks[].field` names; old hyphenated field examples do not describe the current form parser. Embedded services need a separate URL/provider policy, consent and CSP review; do not turn this into a field for arbitrary iframe HTML.

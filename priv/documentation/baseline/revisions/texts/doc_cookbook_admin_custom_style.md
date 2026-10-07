# Add a stylesheet to admin

Place your stylesheet at `priv/lib/css/garden-admin.css` in the site or an active module. Add `priv/templates/_html_head_admin.tpl`:

```django
{% lib "css/garden-admin.css" %}
```

The admin head collects these templates from active applications. Keep selectors scoped to your own widget or a deliberate admin class; broad rules for every input or button can break other modules.

For a reusable module, use an OTP application such as `zotonic_mod_garden_admin`, with `src/zotonic_mod_garden_admin.app.src` and main module `mod_garden_admin.erl`. The application and module names differ intentionally. Follow the module creation task for its complete application metadata and activation.

Compile, enable the module if applicable, reload admin and inspect the stylesheet request. Test narrow and wide screens and the form's error state.

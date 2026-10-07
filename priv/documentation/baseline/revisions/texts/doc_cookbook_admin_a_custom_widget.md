# Add a field to the resource editor

Create `priv/templates/_admin_edit_content_extra.tpl` in the active site or module:

```django
{% extends "admin_edit_widget_std.tpl" %}
{% block widget_id %}garden-note-widget{% endblock %}
{% block widget_title %}{_ Internal note _}{% endblock %}
{% block widget_content %}
    <label for="{{ #note }}">{_ Note _}</label>
    <textarea id="{{ #note }}" name="garden_note" class="form-control"
              {% if not id.is_editable %}disabled{% endif %}>{{ id.garden_note|escape }}</textarea>
{% endblock %}
```

The standard admin layout includes this extension point inside the resource form. Do not nest another form. If the site already has this template, include a separate widget from it instead of overwriting existing content.

Reload an editable practice page, enter a note, save and reopen it. The value should remain. Test a read-only user too. The ordinary resource update checks edit permission; custom server handlers must perform equivalent checks.

“Internal” is only a label here, not field-level secrecy. If this value is confidential, put it behind an appropriate model/storage access policy rather than assuming a custom resource property is private.

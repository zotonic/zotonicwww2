# Update a form field with an action

On a page with the standard Zotonic browser scripts:

```django
<label for="{{ #choice }}">{_ Chosen value _}</label>
<input id="{{ #choice }}" name="choice" value="">
{% button text="Choose garden" action={set_value target=#choice value="Garden"} %}
```

Click the button and expect “Garden” in the field. Generated `#` IDs keep separate instances of the template distinct. If the chooser is inside a dialog, pass the target ID into that dialog template and add `action={dialog_close}` after setting the value.

This changes browser state only. Validate the value again in the server-side form handler before saving. A visitor can change an input regardless of how the chooser populated it.

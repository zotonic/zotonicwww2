---
name: "developer_forms_validation"
title: "Validate a form on both sides"
summary: "Use semantic form controls with labels and meaningful names. Add Zotonic validators for immediate feedback, then validate the submitted values again at the server boundary."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_browser_apis"
order: 5
required_modules: []
source_paths: ["apps/zotonic_mod_wires/src/actions", "apps/zotonic_mod_base/priv/lib/js", "apps/zotonic_mod_mqtt", "apps/zotonic_mod_oauth2"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "forms", "template_validator", "validate"]
---

# Validate a form on both sides

Use semantic form controls with labels and meaningful names. Add Zotonic validators for immediate feedback, then validate the submitted values again at the server boundary.

Test an empty form, invalid input, and a valid submission. Verify that a failed submission leaves the entered values available and explains which field needs attention.

Treat repeated submissions as a normal possibility. A disabled submit button helps interaction but cannot guarantee a write happens only once. Enforce any uniqueness or retry rules in the operation itself.

Start with a named, required email field:

```django
<form id="garden-contact" method="post" action="postback">
    <label for="garden-email">{_ Email _}</label>
    <input id="garden-email" name="email" type="email">
    {% validate id="garden-email" name="email" type={presence} type={email} %}
    <button type="submit">{_ Check address _}</button>
</form>
{% wire id="garden-contact" type="submit" postback={check_email} delegate=`mod_garden` %}
```

Add this clause to the exported `event/2` in `mod_garden`, using the header and declarations from the postback task. Broaden its spec to accept `#postback{} | #submit{}` and separate clauses with a semicolon:

```erlang
event(#submit{message = {check_email, []}}, Context) ->
    case z_context:get_q_validated(<<"email">>, Context) of
        Email when is_binary(Email), Email =/= <<>> ->
            z_render:growl(?__("Address checked; nothing was sent.", Context), Context);
        _ ->
            z_render:growl_error(?__("Enter an email address.", Context), Context)
    end.
```

Zotonic's submit pipeline runs the attached validators before calling the handler. This example checks input and shows feedback; it does not store an address or send mail. Do not treat validation as permission to subscribe or email someone. Check missing, malformed, and valid addresses, and also reject invalid input in any API that calls the same domain operation.

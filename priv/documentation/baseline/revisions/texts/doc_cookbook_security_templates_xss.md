# Render template values safely

Template output is not universally auto-escaped. Treat request values, custom model results and external data as untrusted.

For ordinary text and quoted HTML attributes, escape at the output:

```django
<p>{{ q.search|escape }}</p>
<input name="search" value="{{ q.search|escape }}">
```

Request values can also have unexpected structured shapes. For resource lookups, resolve a scalar identifier and use `m.rsc[value].id`; do not trust arbitrary request data as a resource object. Check authorization on the server before reading private data or changing a resource.

Persisted resource properties normally pass through Zotonic's resource sanitization. This allows intentional rich text such as `id.body` to render as HTML. That does not make every custom model, raw database value or import option safe. Do not apply an HTML-stripping bypass to untrusted content.

JavaScript, CSS and URLs require their own handling. Prefer generated element IDs and the `{% javascript %}` pipeline instead of interpolating text into scripts. Use a JSON encoder when sending JSON. HTML escaping alone cannot prevent an unsafe URL scheme; accept only the schemes and destinations your feature supports.

Test a literal `<script>` string, quotes, ampersands, unexpected list/map input and a `javascript:` URL at each relevant boundary. Expect text to stay text and invalid input to be rejected. A CSP is an additional boundary, not a replacement for correct encoding and validation.

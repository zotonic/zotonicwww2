# Share a value between template components

A variable created inside an included template does not become a global variable for the caller. Calculate the value in the caller and pass it to each component:

```django
{% with id.title as heading %}
    {% include "_garden_heading.tpl" heading=heading %}
    {% include "_garden_details.tpl" heading=heading %}
{% endwith %}
```

Use `tag#with` for a local binding, `tag#include` for a component and `tag#compose` when the caller must supply blocks to that component. Keep related markup and its initialization together where practical.

For browser initialization, use `{% javascript %}` so the normal script pipeline handles it. If data must enter JavaScript, use the JavaScript/JSON encoding appropriate to that context; HTML escaping is not a JavaScript encoder. For example:

```django
<span id="{{ #heading }}">{{ id.title }}</span>
{% javascript %}
    const heading = document.getElementById("{{ #heading }}");
    heading.classList.add("is-ready");
{% endjavascript %}
```

Here the browser reads the already rendered element; resource text is not interpolated into a script literal.

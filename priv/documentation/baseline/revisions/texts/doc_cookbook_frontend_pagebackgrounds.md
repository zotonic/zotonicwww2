# Show a page background image

Create a predicate named `background` with the desired page and image category constraints, then connect an image to a practice page through that predicate. Use the first connected image in the page template:

```django
{% with id.o.background[1] as background_id %}
    {% if background_id %}
        {% image background_id class="page-background" alt="" %}
    {% endif %}
{% endwith %}
```

In the site's stylesheet:

```css
.page-background {
    position: fixed;
    inset: 0;
    width: 100%;
    height: 100%;
    object-fit: cover;
    z-index: -1;
}
```

Adjust the stacking context to your page layout and give the content enough contrast. An empty alt text marks this as decoration; meaningful images need descriptive text in the page. Use a mediaclass to limit generated size for the actual design. Test with no connection, one image, a small screen and a logged-out visitor.

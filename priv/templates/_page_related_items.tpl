{% for related_id in related %}
    {% if related_id.is_visible %}
        <article>
            <p class="related-content__type">{{ related_id.category_id.title }}</p>
            <h3><a href="{{ related_id.page_url }}">{{ related_id.title }}</a></h3>
            <p>{{ related_id|summary:140 }}</p>
        </article>
    {% endif %}
{% endfor %}

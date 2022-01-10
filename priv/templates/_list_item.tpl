{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %} do_clickable">
    <p>
        <a href="{{ id.page_url }}">
            {{ id.title }}.
        </a>
        {{ id|summary:120 }}
    </p>
</div>
{% endif %}

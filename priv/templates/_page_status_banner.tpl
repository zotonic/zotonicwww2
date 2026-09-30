{% if id.is_visible %}
    {% if id.content_group_id.name == 'content_group_deprecated_docs' %}
        <div class="alert alert-warning" role="note" aria-label="{_ Content status _}">
            <strong>{_ Deprecated content _}</strong>
            {_ This page is outdated and is no longer maintained. _}
            {% if not id.is_published %}
                <strong>{_ This page is also unpublished. _}</strong>
            {% endif %}
        </div>
    {% elseif not id.is_published %}
        <div class="alert alert-warning" role="note" aria-label="{_ Content status _}">
            <strong>{_ Unpublished content _}</strong>
            {_ This page is not published and may be incomplete or outdated. _}
        </div>
    {% endif %}
{% endif %}

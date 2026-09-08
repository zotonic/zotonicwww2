{% with q.ids|split:"," as related %}
    {% include "_page_related_items.tpl" related=related %}
{% endwith %}

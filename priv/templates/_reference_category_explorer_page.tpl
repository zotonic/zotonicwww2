{% with m.rsc[q.category_id].id as category_id %}
    {% if category_id %}
        {% with m.zotonicwww2_search.category_page::%{
                category: category_id,
                category_filter: q.category,
                text: q.qs,
                subject: q.subject,
                module: q.module,
                page: q.page,
                limit: 40
            }
            as explorer
        %}
            {% include "_reference_category_explorer_items.tpl"
                explorer=explorer
                category_id=category_id
            %}
        {% endwith %}
    {% endif %}
{% endwith %}

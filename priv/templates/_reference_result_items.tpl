{% for result_id in result_ids %}
    {% if result_id.is_visible %}
        <article class="reference-result">
            <p class="reference-result__meta">
                {{ result_id.category_id.title }}
            </p>
            <h3>
                <a class="reference-result__link" href="{{ result_id.page_url }}">
                    {{ result_id.title|default:_"Untitled" }}
                </a>
            </h3>
            {% if result_id|summary:220 as result_summary %}
                <p class="reference-result__summary">{{ result_summary }}</p>
            {% endif %}
        </article>
    {% endif %}
{% endfor %}

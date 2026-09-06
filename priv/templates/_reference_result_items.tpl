{% for result_id in result_ids %}
    {% if result_id.is_visible %}
        <article class="reference-result">
            <p class="reference-result__meta">
                {{ result_id.category_id.title }}
                {% if result_id.o.in_module[1] as module_id %}
                    <span aria-hidden="true">&middot;</span>
                    <a href="{{ module_id.page_url }}">{{ module_id.title }}</a>
                {% endif %}
            </p>
            <h3>
                <a class="reference-result__link" href="{{ result_id.page_url }}">
                    {{ result_id.title|default:_"Untitled" }}
                </a>
            </h3>
            {% if result_id|summary:220 as result_summary %}
                <p class="reference-result__summary">{{ result_summary }}</p>
            {% endif %}
            {% if result_id.o.subject as subjects %}
                <ul class="subject-label-list reference-result__subjects" aria-label="{_ Topics _}">
                    {% for subject_id in subjects|slice:[6] %}
                        <li>{% include "_subject_label.tpl" subject_id=subject_id %}</li>
                    {% endfor %}
                </ul>
            {% endif %}
        </article>
    {% endif %}
{% endfor %}

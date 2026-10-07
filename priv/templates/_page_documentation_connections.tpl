{# Editorial suggestions and further reading retain their outgoing edge order.
   Keep automatic refers tracking and subject-based suggestions separate. #}
{% with id.o.relation|is_visible as related %}
    {% if related %}
        <section class="connections related-pages" aria-labelledby="related-tasks-{{ id }}">
            <h2 id="related-tasks-{{ id }}">{_ Related pages _}</h2>
            <ul class="related-pages__grid">
                {% for target in related %}
                    <li>
                        <a class="related-pages__card" href="{{ target.page_url }}">
                            <span class="related-pages__text">
                                <strong>{{ target.title }}</strong>
                                {% if target.summary %}
                                    <span class="related-pages__summary">{{ target.summary|striptags|truncate:160 }}</span>
                                {% endif %}
                            </span>
                            <span class="related-pages__arrow" aria-hidden="true">→</span>
                        </a>
                    </li>
                {% endfor %}
            </ul>
        </section>
    {% endif %}
{% endwith %}
{% with id.o.hasreference|is_visible as references %}
    {% if references %}
        <section class="connections" aria-labelledby="further-reading-{{ id }}">
            <h2 id="further-reading-{{ id }}">{_ Further reading _}</h2>
            <ul>
                {% for target in references %}
                    <li><a href="{{ target.page_url }}">{{ target.title }}</a></li>
                {% endfor %}
            </ul>
        </section>
    {% endif %}
{% endwith %}

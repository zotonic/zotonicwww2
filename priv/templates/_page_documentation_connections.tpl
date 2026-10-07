{# Editorial suggestions and further reading retain their outgoing edge order.
   Keep automatic refers tracking and subject-based suggestions separate. #}
{% with id.o.relation|is_visible as related %}
    {% if related %}
        <section class="connections related-pages" aria-labelledby="related-tasks-{{ id }}">
            <h2 id="related-tasks-{{ id }}">{_ Related pages _}</h2>
            {% include "_documentation_link_cards.tpl" targets=related %}
        </section>
    {% endif %}
{% endwith %}
{% with id.o.hasreference|is_visible as references %}
    {% if references %}
        <section class="connections related-pages further-reading" aria-labelledby="further-reading-{{ id }}">
            <h2 id="further-reading-{{ id }}">{_ Further reading _}</h2>
            {% include "_documentation_link_cards.tpl" targets=references %}
        </section>
    {% endif %}
{% endwith %}

<div class="page-relations subject-topic-category">
    {% if id.name == 'keyword' %}
        {% for facet in m.category.keyword.tree_flat %}
            {% if facet.id != id and facet.id.subject_topic_facet_category %}
                <section class="connections" aria-labelledby="subject-facet-{{ facet.id }}">
                    <h2 id="subject-facet-{{ facet.id }}">
                        <a href="{{ facet.id.page_url }}">{{ facet.id.title }}</a>
                    </h2>
                    {% if facet.id.summary %}
                        <p>{{ facet.id.summary }}</p>
                    {% endif %}
                    {% include "_subject_topic_keyword_links.tpl" facet_id=facet.id %}
                </section>
            {% endif %}
        {% empty %}
            <p>{_ No subject keywords have been imported yet. _}</p>
        {% endfor %}
    {% else %}
        <section class="connections" aria-labelledby="subject-facet-keywords">
            <h2 id="subject-facet-keywords">{_ Keywords _}</h2>
            {% include "_subject_topic_keyword_links.tpl" facet_id=id %}
        </section>
    {% endif %}
</div>

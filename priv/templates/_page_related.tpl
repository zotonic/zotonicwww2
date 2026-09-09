{% if id.is_a.text %}
    {# Keep one ranked set so equally ranked results cannot move between the
       initial list and the lazily rendered remainder. #}
    {% with m.search.match_objects::%{
        id: id,
        predicate: "subject",
        cat: "text",
        pagelen: 36
    } as related %}
        {% if related %}
            <section class="related-content" aria-labelledby="related-content-title">
                <h2 id="related-content-title">{_ Related documentation _}</h2>
                <div class="related-content__grid">
                    {% include "_page_related_items.tpl" related=related.result|slice:[9] %}

                    {% with related.result|slice:[10,36] as additional %}
                        {% if additional %}
                            <button id="related-content-more-{{ id }}"
                                    class="related-content__more"
                                    type="button"
                                    data-onclick-topic="model/loadmore/post/replace"
                                    data-template="_page_related_more.tpl"
                                    data-url="{% url none ids=additional|join:',' %}"
                                    data-replace-location="false">
                                {_ Show more _}
                            </button>
                        {% endif %}
                    {% endwith %}
                </div>
            </section>
        {% endif %}
    {% endwith %}
{% endif %}

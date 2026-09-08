{% with search.facets.category.counts|slice:[8] as categories %}
{% with search.facets.subject.counts|slice:[8] as subjects %}
{% with search.facets.module.counts|slice:[8] as modules %}
    {% if categories or subjects or modules or search.category or search.subject or search.module %}
        <div class="search-facets" aria-label="{_ Filter search results _}">
            {% if categories %}
                <div class="search-facet-group">
                    <p>{_ Category _}</p>
                    <div class="search-facet-options">
                        {% for facet in categories %}
                            <a href="{% url search qs=search.query category=facet.value subject=search.subject module=search.module %}"
                               class="search-facet{% if search.category == facet.value %} is-active{% endif %}"
                               data-search-facet="category" data-search-value="{{ facet.value }}"
                               data-search-form="{{ search_form_id }}">{{ facet.label }} <span>{{ facet.count }}</span></a>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if subjects %}
                <div class="search-facet-group">
                    <p>{_ Topic _}</p>
                    <div class="search-facet-options">
                        {% for facet in subjects %}
                            <a href="{% url search qs=search.query category=search.category subject=facet.value module=search.module %}"
                               class="search-facet{% if search.subject == facet.value %} is-active{% endif %}"
                               data-search-facet="subject" data-search-value="{{ facet.value }}"
                               data-search-form="{{ search_form_id }}">{{ facet.label }} <span>{{ facet.count }}</span></a>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if modules %}
                <div class="search-facet-group">
                    <p>{_ Module _}</p>
                    <div class="search-facet-options">
                        {% for facet in modules %}
                            <a href="{% url search qs=search.query category=search.category subject=search.subject module=facet.value %}"
                               class="search-facet{% if search.module == facet.value %} is-active{% endif %}"
                               data-search-facet="module" data-search-value="{{ facet.value }}"
                               data-search-form="{{ search_form_id }}">{{ facet.label }} <span>{{ facet.count }}</span></a>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if search.category or search.subject or search.module %}
                <a class="search-facets__clear" href="{% url search qs=search.query %}"
                   data-search-clear data-search-form="{{ search_form_id }}">{_ Clear filters _}</a>
            {% endif %}
        </div>
    {% endif %}
{% endwith %}
{% endwith %}
{% endwith %}

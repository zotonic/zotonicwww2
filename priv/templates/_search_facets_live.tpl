{% with search.facets.category.counts|slice:[8] as categories %}
{% with search.facets.subject.counts|slice:[8] as subjects %}
{% with search.facets.module.counts|slice:[8] as modules %}
    {% if categories or subjects or modules or search.category or search.subject or search.module %}
        <form class="search-facets do_forminit"
              action="{% url search %}"
              method="get"
              aria-label="{_ Filter search results _}"
              data-onsubmit-topic="model/location/post/qlist/submit"
              data-oninput-topic="model/location/post/qlist/submit">
            <input type="hidden" name="qs" value="{{ search.query|escape }}">

            {% if categories or search.category %}
                <div class="search-facet-group">
                    <p>{_ Category _}</p>
                    <div class="search-facet-options">
                        <label class="search-facet{% if not search.category %} is-active{% endif %}">
                            <input type="radio" name="category" value=""
                                   {% if not search.category %}checked{% endif %}>
                            {_ All categories _}
                        </label>
                        {% for facet in categories %}
                            <label class="search-facet{% if search.category == facet.value %} is-active{% endif %}">
                                <input type="radio" name="category" value="{{ facet.value|escape }}"
                                       {% if search.category == facet.value %}checked{% endif %}>
                                {{ facet.label|escape }} <span>{{ facet.count }}</span>
                            </label>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if subjects or search.subject %}
                <div class="search-facet-group">
                    <p>{_ Topic _}</p>
                    <div class="search-facet-options">
                        <label class="search-facet{% if not search.subject %} is-active{% endif %}">
                            <input type="radio" name="subject" value=""
                                   {% if not search.subject %}checked{% endif %}>
                            {_ All topics _}
                        </label>
                        {% for facet in subjects %}
                            <label class="search-facet{% if search.subject == facet.value %} is-active{% endif %}">
                                <input type="radio" name="subject" value="{{ facet.value|escape }}"
                                       {% if search.subject == facet.value %}checked{% endif %}>
                                {{ facet.label|escape }} <span>{{ facet.count }}</span>
                            </label>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if modules or search.module %}
                <div class="search-facet-group">
                    <p>{_ Module _}</p>
                    <div class="search-facet-options">
                        <label class="search-facet{% if not search.module %} is-active{% endif %}">
                            <input type="radio" name="module" value=""
                                   {% if not search.module %}checked{% endif %}>
                            {_ All modules _}
                        </label>
                        {% for facet in modules %}
                            <label class="search-facet{% if search.module == facet.value %} is-active{% endif %}">
                                <input type="radio" name="module" value="{{ facet.value|escape }}"
                                       {% if search.module == facet.value %}checked{% endif %}>
                                {{ facet.label|escape }} <span>{{ facet.count }}</span>
                            </label>
                        {% endfor %}
                    </div>
                </div>
            {% endif %}

            {% if search.category or search.subject or search.module %}
                <a class="search-facets__clear"
                   href="{% url search qs=search.query %}"
                   data-onclick-topic="model/location/post/push">{_ Clear filters _}</a>
            {% endif %}
        </form>
    {% endif %}
{% endwith %}
{% endwith %}
{% endwith %}

{% if search.query|length < 2 %}
    <div class="search-empty search-empty--prompt">
        <p class="search-empty__label">{_ Search across all Zotonic content _}</p>
        <h2>{_ Type at least two characters _}</h2>
        <p>{_ Results are matched against titles, summaries and topics as you type. _}</p>
    </div>
{% else %}
    <header class="search-summary">
        <div>
            <p class="search-summary__label">{_ Results for _}</p>
            <h2>&ldquo;{{ search.query|escape }}&rdquo;</h2>
        </div>
        {% if search.is_fallback %}
            <p class="search-summary__mode">
                {_ Showing close matches first, followed by broader full-text results. _}
            </p>
        {% endif %}
    </header>

    {% if is_live_query %}
        {% include "_search_facets_live.tpl" search=search %}
    {% else %}
        {% include "_search_facets.tpl"
            search=search
            search_form_id=search_form_id
        %}
    {% endif %}

    {% if search.result_ids %}
        <div class="search-result-list">
            {% if is_overlay %}
                {% include "_search_results_items.tpl" search=search hide_loader %}
            {% else %}
                {% include "_search_results_items.tpl" search=search %}
            {% endif %}
        </div>

        {% if is_overlay %}
            <a class="search-view-all" href="{% url search qs=search.query category=search.category subject=search.subject module=search.module %}">
                {_ Open full search _} <span aria-hidden="true">&rarr;</span>
            </a>
        {% endif %}
    {% else %}
        <div class="search-empty">
            <p class="search-empty__label">{_ No results _}</p>
            <h2>{_ Try another phrase _}</h2>
            <p>{_ Use a module name, a task such as “send email”, or a broader topic. _}</p>
        </div>
    {% endif %}
{% endif %}

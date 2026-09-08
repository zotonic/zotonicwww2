{% with m.zotonicwww2_search.category::%{
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
<section class="reference-explorer" aria-labelledby="reference-explorer-title">
    <header class="reference-explorer__header">
        <div>
            <p class="reference-explorer__eyebrow">{_ Browse the reference _}</p>
            <h2 id="reference-explorer-title">{_ Explore _} {{ category_id.title }}</h2>
        </div>
        <p class="reference-explorer__count">
            {{ explorer.total }}
            {% if explorer.total == 1 %}{_ item _}{% else %}{_ items _}{% endif %}
        </p>
    </header>

    <form class="reference-explorer__form do_forminit"
          action="{{ category_id.page_url }}"
          method="get"
          data-onsubmit-topic="model/location/post/qlist/submit"
          data-oninput-topic="model/location/post/qlist/submit">
        <div class="reference-explorer__search">
            <label for="{{ #query }}">{_ Search in _} {{ category_id.title }}</label>
            <div>
                <input id="{{ #query }}" type="search" name="qs" value="{{ explorer.query|escape }}"
                       placeholder="{_ Search by name or description _}">
            </div>
        </div>

        {% with explorer.categories as category_facets %}
        {% if category_id.name == 'reference' and category_facets or explorer.subject_groups or explorer.modules %}
            <div class="reference-explorer__facets">
                {% if category_id.name == 'reference' and category_facets %}
                    <details class="reference-facet-group reference-facet-group--category" open>
                        <summary>
                            <span>{_ Category _}</span>
                            <span>{{ category_facets|length }}</span>
                        </summary>
                        <div class="reference-facet-options">
                            <label class="reference-facet{% if not explorer.selected_category %} is-active{% endif %}">
                                <input type="radio" name="category" value=""
                                       {% if not explorer.selected_category %}checked{% endif %}>
                                <span>{_ All categories _}</span>
                            </label>
                            {% for facet in category_facets %}
                                <label class="reference-facet{% if explorer.selected_category == facet.value %} is-active{% endif %}">
                                    <input type="radio" name="category" value="{{ facet.value }}"
                                           {% if explorer.selected_category == facet.value %}checked{% endif %}>
                                    <span>
                                        {% if facet.value == category_id %}
                                            {_ General reference _}
                                        {% else %}
                                            {{ facet.label }}
                                        {% endif %}
                                    </span>
                                    <small>{{ facet.count }}</small>
                                </label>
                            {% endfor %}
                        </div>
                    </details>
                {% endif %}

                {% if explorer.subject_groups %}
                    <div class="reference-facet-reset">
                        <label class="reference-facet{% if not explorer.subject %} is-active{% endif %}">
                            <input type="radio" name="subject" value=""
                                   {% if not explorer.subject %}checked{% endif %}>
                            <span>{_ All topics _}</span>
                        </label>
                    </div>
                {% endif %}
                {% for group in explorer.subject_groups %}
                    <details class="reference-facet-group"
                             {% if group.key == 'domain' or group.key == 'architecture' or explorer.subject.category_id == group.category_id %}open{% endif %}>
                        <summary>
                            <span>{{ group.category_id.title }}</span>
                            <span>{{ group.counts|length }}</span>
                        </summary>
                        {% if group.category_id.summary %}
                            <p>{{ group.category_id.summary }}</p>
                        {% endif %}
                        <div class="reference-facet-options">
                            {% for facet in group.counts %}
                                <label class="reference-facet subject-label--{{ group.key }}{% if explorer.subject == facet.value %} is-active{% endif %}">
                                    <input type="radio" name="subject" value="{{ facet.value }}"
                                           {% if explorer.subject == facet.value %}checked{% endif %}>
                                    <span>{{ facet.label }}</span>
                                    <small>{{ facet.count }}</small>
                                </label>
                            {% endfor %}
                        </div>
                    </details>
                {% endfor %}

                {% if explorer.modules %}
                    <details class="reference-facet-group" {% if explorer.module %}open{% endif %}>
                        <summary>
                            <span>{_ Module _}</span>
                            <span>{{ explorer.modules|length }}</span>
                        </summary>
                        <div class="reference-facet-options">
                            <label class="reference-facet{% if not explorer.module %} is-active{% endif %}">
                                <input type="radio" name="module" value=""
                                       {% if not explorer.module %}checked{% endif %}>
                                <span>{_ All modules _}</span>
                            </label>
                            {% for facet in explorer.modules %}
                                <label class="reference-facet{% if explorer.module == facet.value %} is-active{% endif %}">
                                    <input type="radio" name="module" value="{{ facet.value }}"
                                           {% if explorer.module == facet.value %}checked{% endif %}>
                                    <span>{{ facet.label }}</span>
                                    <small>{{ facet.count }}</small>
                                </label>
                            {% endfor %}
                        </div>
                    </details>
                {% endif %}
            </div>

            {% if explorer.selected_category or explorer.subject or explorer.module or explorer.query %}
                <div class="reference-explorer__actions">
                    <a href="{{ category_id.page_url }}"
                       data-onclick-topic="model/location/post/push">{_ Clear filters _}</a>
                </div>
            {% endif %}
        {% endif %}
        {% endwith %}
    </form>

    {% if explorer.result_ids %}
        <div class="reference-result-list" aria-live="polite">
            {% with q.page|default:1|to_integer as current_page %}
                {% if is_new_query and current_page > 1 %}
                    {% for previous_page in 1|range:(current_page-1) %}
                        {% with m.zotonicwww2_search.category_page::%{
                                category: category_id,
                                category_filter: q.category,
                                text: q.qs,
                                subject: q.subject,
                                module: q.module,
                                page: previous_page,
                                limit: 40
                            }
                            as previous_explorer
                        %}
                            {% include "_reference_category_explorer_items.tpl"
                                explorer=previous_explorer
                                category_id=category_id
                                hide_loader
                            %}
                        {% endwith %}
                    {% endfor %}
                {% endif %}
            {% endwith %}
            {% include "_reference_category_explorer_items.tpl"
                explorer=explorer
                category_id=category_id
            %}
        </div>
    {% else %}
        <div class="reference-explorer__empty">
            <h3>{_ No matching documentation _}</h3>
            <p>{_ Try a broader keyword or clear the filters. _}</p>
        </div>
    {% endif %}
</section>
{% endwith %}

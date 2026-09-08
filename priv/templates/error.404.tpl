{% extends "base.tpl" %}

{% block title %}{_ Page not found _} — {{ m.site.title }}{% endblock %}

{% block body_class %}error-page error-page--404{% endblock %}

{% block content %}
    {% with m.zotonicwww2_search.path_query::%{ path: zotonic_dispatch_path } as path_query %}
        <article class="not-found-intro">
            <p class="not-found-intro__eyebrow">{% block error_code %}404{% endblock %}</p>
            <h1>{% block error_heading %}{_ This page has moved or does not exist _}{% endblock %}</h1>
            <p>
                {% block error_message %}{_ We used the words in its address to look for useful documentation. You can refine the search below. _}{% endblock %}
            </p>

            <form class="search-page-form"
                  action="{% url search %}"
                  method="get"
                  role="search">
                <label class="sr-only" for="not-found-search-query">{_ Text to search _}</label>
                <input id="not-found-search-query"
                       name="qs"
                       type="search"
                       value="{{ path_query|escape }}"
                       placeholder="{_ Search Zotonic documentation _}"
                       autocomplete="off">
                <button type="submit">{_ Search _}</button>
            </form>
        </article>

        {% if path_query|length >= 2 %}
            {% with m.zotonicwww2_search.results::%{
                    text: path_query,
                    limit: 8
                }
                as search
            %}
                <section class="not-found-results" aria-labelledby="not-found-results-title">
                    <header class="search-summary">
                        <div>
                            <p class="search-summary__label">{_ Suggested pages _}</p>
                            <h2 id="not-found-results-title">&ldquo;{{ search.query|escape }}&rdquo;</h2>
                        </div>
                        {% if search.is_fallback %}
                            <p class="search-summary__mode">
                                {_ Showing close matches first, followed by broader full-text results. _}
                            </p>
                        {% endif %}
                    </header>

                    {% if search.result_ids %}
                        <div class="search-result-list">
                            {% include "_search_results_items.tpl" search=search hide_loader %}
                        </div>
                        <a class="search-view-all" href="{% url search qs=search.query %}">
                            {_ Open full search _} <span aria-hidden="true">&rarr;</span>
                        </a>
                    {% else %}
                        <div class="search-empty">
                            <p class="search-empty__label">{_ No close matches _}</p>
                            <h2>{_ Try a shorter phrase _}</h2>
                            <p>
                                {_ Edit the search above, or return to the _}
                                <a href="{{ m.rsc.page_home.page_url }}">{_ homepage _}</a>.
                            </p>
                        </div>
                    {% endif %}
                </section>
            {% endwith %}
        {% endif %}
    {% endwith %}
{% endblock %}

{% extends "base.tpl" %}

{% block title %}
    {_ Search _}{% if q.qs %}: {{ q.qs|escape }}{% endif %}
{% endblock %}

{% block body_class %}search{% endblock %}

{% block content %}
    <article class="search-page-intro">
        <p class="search-page-intro__eyebrow">{_ Find your way through Zotonic _}</p>
        <h1>{_ Search documentation _}</h1>
        <p>{_ Search guides, reference documentation, release notes, articles and cookbook recipes. _}</p>

        <form id="search-form" class="search-page-form do_forminit"
              action="{% url search %}"
              method="get"
              role="search"
              data-onsubmit-topic="model/location/post/qlist/submit"
              data-oninput-topic="model/location/post/qlist/submit">
            <label class="sr-only" for="search-query">{_ Text to search _}</label>
            <input id="search-query" name="qs" type="search"
                   placeholder="{_ What do you want to build? _}"
                   value="{{ q.qs|escape }}" autocomplete="off" autofocus>
            <span id="{{ #search_form_facets }}">
                {% include "_search_form_facets.tpl" %}
            </span>
            <button type="submit">{_ Search _}</button>
        </form>
    </article>

    <div id="{{ #search_results }}" class="search-feedback"
         aria-live="polite">
        {% include "_search_results.tpl" %}
    </div>

    {% live
        topic="model/location/event/qlist"
        template="_search_results.tpl"
        target=#search_results
        method="patch"
    %}
    {% live
        topic="model/location/event/qlist"
        template="_search_form_facets.tpl"
        target=#search_form_facets
        method="patch"
    %}
{% endblock %}

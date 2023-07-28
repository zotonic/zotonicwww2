{% extends "base.tpl" %}

{% block title %}
    {_ Search _}: {{ q.qs|escape }}
{% endblock %}

{% block body_class %}search{% endblock %}

{% block content %}
    <article>
        <h1>{_ Search _}</h1>

        <form id="search-form" class="search-form" action="{% url search %}" method="GET">
            <p class="label-floating">
                <input placeholder="{_ Text to search _}" name="qs" class="form-control" name="qs" autofocus value="{{ q.qs|escape }}">
            </p>
        </form>
    </article>

    <div id="search-results"
         class="do_feedback"
         data-feedback="trigger: 'search-form', template: '_search_results.tpl'">
        {% include "_search_results.tpl" %}
    </div>
{% endblock %}
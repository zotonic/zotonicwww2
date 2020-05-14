{% extends "page.tpl" %}

{% block content %}
    {# Replace this with a generic documention image, based on the current category #}
    {% if id.depiction %}
        <div class="page-header" style="background-image: url({% image_url id.depiction mediaclass='page-header' %})">
            <h1>{{ id.title }}</h1>
        </div>
    {% else %}
        <h1>{{ id.title }} <span class="text-muted">{{ id.category_id.title|lower }}</span></h1>
    {% endif %}

    <p class="in-module">
        {% if id.github_url %}
            <a class="edit-github" href="{{ id.github_url }}" target="_blank">
                <span class="fa fa-github"></span> {_ Edit on GitHub _}
            </a>
        {% endif %}
        {% if id.o.in_module[1] as module_id %}
            <a href="{{ module_id.page_url }}">
                <span>{_ Module: _}</span> {{ module_id.title }}
            </a>
        {% else %}
            &nbsp;
        {% endif %}
    </p>

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>
{% endblock %}


{% extends "page.tpl" %}

{% block content %}
    {% if id.depiction %}
        <div class="page-header" style="background-image: url({% image_url id.depiction mediaclass='page-header-color' %})">
            <h1>{{ id.title }}</h1>
        </div>
    {% else %}
        <h1>{{ id.title }}</h1>
    {% endif %}

    <p class="summary">
        {{ id.summary }}
    </p>

    <p>
        <a href="{{ id.website }}" target="_blank"><span class="fa fa-external-link"></span> {{ id.website }}</a>
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>
{% endblock %}


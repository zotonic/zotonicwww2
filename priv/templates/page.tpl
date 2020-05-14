{% extends "base.tpl" %}

{% block content %}
    {% if id.depiction %}
        <div class="page-header" style="background-image: url({% image_url id.depiction mediaclass='page-header' %})">
            <h1>{{ id.title }}</h1>
        </div>
    {% else %}
        <h1>{{ id.title }}</h1>
    {% endif %}

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>
{% endblock %}

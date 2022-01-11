{% extends "page.tpl" %}

{% block content %}

    <p>{{ id.category_id.title }}</p>

    <h1>{{ id.title }}</h1>

    {% if id.depiction as dep %}
        {% include "_body_media.tpl" id=dep.id size="large" mediaclass="body-media-large" caption="-" %}
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

{% block content_after %}
    <div class="page-relations">
        <h3>{_ More made with Zotonic _}</h3>
        <div class="list-items">
            {% for id in m.search[{query cat=id.category_id pagelen=5 sort="random" id_exclude=id}] %}
                {% catinclude "_list_item.tpl" id %}
            {% endfor %}
        </div>
    </div>
{% endblock %}

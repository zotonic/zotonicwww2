{#
    Page for media items.
    Media can be images, videos and documents.

    In this case we display a big preview image and a download.
    If there is a website then we display a link to the website.
#}
{% extends "base.tpl" %}

{% block content %}
    {% if not id.is_a.document %}
        {% include "_body_media.tpl" id=id mediaclass="body-media-large" %}
    {% endif %}

    <h1>{{ id.title }}</h1>

    {% if id.is_a.document %}
        {% include "_body_media.tpl" id=id mediaclass="media-preview" align="left" caption='-' link={media_inline id=id}|url %}
    {% endif %}

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>

    {% if id.website %}
        <p>
            <a href="{{ id.website }}" target="_blank"><span class="fa fa-external-link"></span> {{ id.website }}</a>
        </p>
    {% endif %}

    {% if id.medium as medium %}
        {% if medium.size > 0 %}
            <p class="text-muted">
                {# Medium properties are not sanitized, so be careful to escape them #}
                {{ medium.mime|escape }} {{ medium.size|filesizeformat }}
            </p>
            <p>
                <a href="{% url media_inline id=id %}" target="_blank" class="btn btn-primary">{_ Download in new window _}</a>
            </p>
        {% endif %}
    {% endif %}

{% endblock %}

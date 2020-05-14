{% extends "base.tpl" %}

{% block content %}
    {% include "_body_media.tpl" id=id mediaclass="body-media-large" %}

    <h1>{{ id.title }}</h1>

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>
{% endblock %}

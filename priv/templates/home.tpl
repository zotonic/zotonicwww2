{% extends "base.tpl" %}

{% block title %}{{ m.site.title }}{% endblock %}

{% block content %}

    <article>
        <h1>{{ id.title }}</h1>

        <div class="subtitle">
            {{ id.body }}
        </div>
    </article>

{% endblock %}

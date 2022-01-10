{% extends "page.tpl" %}

{% block content %}
<article>
    {# Replace this with a generic documention image, based on the current category #}
    {% if id.depiction %}
        <div class="page-header" style="background-image: url({% image_url id.depiction mediaclass='page-header' %})">
            <h1>{{ id.title }}</h1>
        </div>
    {% else %}
        <h1>{{ id.title }}</h1>
    {% endif %}

    {% if id.o.in_module[1] as module_id %}
        <div class="admonition note">
            <p class="first admonition-title">{_ Module _}</p>
            <p class="last"><a class="reference internal" href="/id/doc_template_filter_filter_is_not_a#filter-is-not-a">
                <a href="{{ module_id.page_url }}">{{ module_id.title }}</a>
            </p>
        </div>
    {% endif %}

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>


    {% if id.github_url %}
        <p class="edit-github">
            <a href="{{ id.github_url }}" target="_blank">
                <span class="fa fa-github"></span> {_ Edit on GitHub _}
            </a>
        </p>
    {% endif %}
</article>
{% endblock %}


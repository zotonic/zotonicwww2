{% extends "page.tpl" %}

{% block content %}
    <article>
        {% include "_page_meta.tpl" %}
        <h1{% if id.is_a.category %} class="category-page__title"{% endif %}>{{ id.title }} {% include "_doc_title_suffix.tpl" id=id %}</h1>

        {% if id.is_a.category %}
            {% include "_category_tree_navigation.tpl" id=id %}
        {% endif %}

        {% if id.depiction as dep %}
            {% include "_body_media.tpl" id=dep.id %}
        {% endif %}

        {% if id.o.in_module[1] as module_id %}
            <aside class="admonition note">
                <p class="first admonition-title">{_ Module _}</p>
                <p class="last"><a href="{{ module_id.page_url }}">{{ module_id.title }}</a></p>
            </aside>
        {% endif %}

        <p class="summary">
            {{ id.summary }}
        </p>

        {% block content_before_body %}{% endblock %}

        {% include "_page_body.tpl" id=id body=id.body|zotonicwww2_without_title:id.title %}

        {# Reference documentation and release notes are maintained on GitHub #}
        {% if id.github_url and (id.is_a.reference or id.is_a.releasenotes) %}
            <p class="edit-github">
                <a href="{{ id.github_url }}"
                   target="_blank" rel="noopener">
                    <span class="fa fa-github"></span> {_ Edit on GitHub _}
                </a>
            </p>
        {% endif %}
    </article>
{% endblock %}

{# Category navigation is rendered beside the page heading above. #}
{% block category_navigation %}{% endblock %}

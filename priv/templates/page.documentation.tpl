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

        {% if id.is_external_module %}
            <aside class="admonition external-module">
                <p class="first admonition-title">{_ External module _}</p>
                <p>{_ This documentation is provided by a third-party module. _} {{ id.external_module_title }}</p>
                <p class="last">
                    <a href="{{ id.git_url }}" rel="noopener">{_ Git repository _}</a>
                    {% if id.website_url and id.website_url != id.git_url %} · <a href="{{ id.website_url }}" rel="noopener">{_ More information _}</a>{% endif %}
                    {% if id.hex_package %} · <a href="https://hex.pm/packages/{{ id.hex_package|urlencode }}">{_ Hex package _}: {{ id.hex_package }}</a>{% endif %}
                </p>
            </aside>
        {% endif %}

        {% if id.o.in_module[1] as module_id %}
            <aside class="admonition {% if id.is_external_module %}external-module{% else %}note{% endif %}">
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
        {% if not id.is_external_module and id.github_url and (id.is_a.reference or id.is_a.releasenotes) %}
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

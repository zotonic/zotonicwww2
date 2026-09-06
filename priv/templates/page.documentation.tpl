{% extends "page.tpl" %}

{% block content %}
    <article>
        {% include "_page_meta.tpl" %}
        <h1>{{ id.title }}</h1>

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

        <div class="body">
            {{ id.body|show_media }}
        </div>

        {# Reference documentation and release notes are maintained on GitHub #}
        {% if id.github_url and (id.is_a.reference or id.is_a.releasenotes) %}
            <p class="edit-github">
                <a href="{% if id.doc_source_path %}https://github.com/zotonic/zotonic/blob/master/{{ id.doc_source_path|escape }}{% else %}{{ id.github_url|replace:"https://github\\.com/zotonic/zotonic/(blob|edit)/[^/]+/":"https://github.com/zotonic/zotonic/\\1/master/"|escape }}{% endif %}"
                   target="_blank" rel="noopener">
                    <span class="fa fa-github"></span> {_ Edit on GitHub _}
                </a>
            </p>
        {% endif %}
    </article>
{% endblock %}

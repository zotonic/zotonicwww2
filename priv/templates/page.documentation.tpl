{% extends "base.tpl" %}

{% block content %}
    {# Replace this with a generic documention image, based on the current category #}
    {% if id.depiction %}
        <div class="page-header" style="background-image: url({% image_url id.depiction mediaclass='page-header' %})">
            <h1>{{ id.title }}</h1>
        </div>
    {% else %}
        <h1>{{ id.title }} <span class="text-muted">{{ id.category_id.title|lower }}</span></h1>
    {% endif %}

    <p class="in-module">
        {% if id.github_url %}
            <a class="edit-github" href="{{ id.github_url }}" target="_blank">
                <span class="fa fa-github"></span> {_ Edit on GitHub _}
            </a>
        {% endif %}
        {% if id.o.in_module[1] as module_id %}
            <a href="{{ module_id.page_url }}">
                <span>{_ Module: _}</span> {{ module_id.title }}
            </a>
        {% else %}
            &nbsp;
        {% endif %}
    </p>

    <p class="summary">
        {{ id.summary }}
    </p>

    <div class="body">
        {{ id.body|show_media }}
    </div>
{% endblock %}

{% block content_after %}
<div class="page-relations">
    {% if id.o.references as refo %}
        <div class="connections paged" id="content-pager">
            <div class="page-count">
                {{ refo|length }} <span>{_ Pages _}</span>
            </div>
            <div class="list-items">
                {% for id in refo|sort:`title` %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}

    {% with id.o.relation as relo %}
    {% with id.s.relation as rels %}
        {% if relo or rels %}
            <div class="connections">
                <h3>&#x21C4; {_ See also _}</h3>

                <div class="list-items">
                    {% for id in relo %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                    {% for id in rels %}
                        {% if not id|member:relo %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}
    {% endwith %}
    {% endwith %}

    {% if id.s.references as refs %}
        <div class="connections">
            <h3>&rarr; {_ Referred by _}</h3>
            <div class="list-items">
                {% for id in refs %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}


    <div class="connections">
        <h3>&#8712; {{ id.category_id.title }} <span class="text-muted">{_ Category _}</span></h3>

        <div class="list-items">
            {% catinclude "_list_item.tpl" id.category_id %}
        </div>
    </div>
</div>
{% endblock %}

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


{% block content_after %}
<div class="page-relations">
    {% if id.o.haspart as haspart %}
        <div class="connections paged" id="content-pager">
            <div class="page-count">
                {{ haspart|length }} <span>{_ Pages _}</span>
            </div>
            <div class="list-items">
                {% for id in haspart %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}

    {% with id.o.relation as relo %}
    {% with id.s.relation as rels %}
    {% with id.s.haspart -- [ id.category_id ] as hasparts %}
    {% with id.s.haspart.o.haspart as hassibling %}
        {% if relo or rels or hasparts %}
            <div class="connections">
                <h3>&#x21C4; {_ See also _}</h3>

                <div class="list-items">
                    {% for id in hasparts %}
                        {% catinclude "_list_item.tpl" id is_highlight %}
                    {% endfor %}
                    {% for id in relo %}
                        {% if not id|member:hasparts %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                    {% for id in rels %}
                        {% if  not id|member:hasparts
                           and not id|member:relo
                        %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                    {% for id in hassibling %}
                        {% if  not id|member:hasparts
                           and not id|member:relo
                           and not id|member:rels
                        %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}
    {% endwith %}
    {% endwith %}
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

    {% if id.o.in_module as module %}
        <div class="connections">
            <h3>&#8776; {_ Module _} <span class="text-muted"> / {{ id.category_id.title|lower }} </span></h3>
            <div class="list-items">
                {% for id in module %}
                    {% catinclude "_list_item.tpl" id is_highlight %}
                {% endfor %}
                {% for id in m.search[
                    {query cat=id.category_id
                           id_exclude=id
                           hasobject=[module[1], "in_module"]
                           sort="pivot_title"
                    }] %}
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

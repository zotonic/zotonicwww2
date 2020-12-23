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

{% block content_after %}
<div class="page-relations clearfix">
    {% with id.o.relation as relo %}
    {% with id.s.relation as rels %}
    {% with id.s.haspart -- [ id.category_id ] as hasparts %}
    {% with hasparts[1].o.haspart as hassibling %}
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

    <div class="connections">
        <h3>{_ Latest in _} {{ id.category_id.title }}</h3>
        <div class="list-items">
            {% for id in m.search[{latest cat=id.category_id pagelen=20}] %}
                {% catinclude "_list_item.tpl" id %}
            {% endfor %}
        </div>
    </div>

    <div class="connections">
        <h3>&#8712; {{ id.category_id.title }} <span class="text-muted">{_ Category _}</span></h3>
        <div class="list-items">
            {% catinclude "_list_item.tpl" id.category_id %}
        </div>
    </div>

</div>
{% endblock %}
{% extends "base.tpl" %}

{% block content_before %}
    {% include "_guide_navigation.tpl" id=id %}
{% endblock %}

{% block content %}
    <article>
        {% include "_page_meta.tpl" %}
        <h1>{{ id.title }}</h1>

        {% if id.depiction as dep %}
            {% include "_body_media.tpl" id=dep.id %}
        {% endif %}

        <p class="summary">
            {{ id.summary }}
        </p>

        {% include "_page_body.tpl" id=id body=id.body|zotonicwww2_without_title:id.title %}
    </article>
{% endblock %}


{% block content_after %}
<div class="page-relations">

    {% with m.zotonicwww2_guide.navigation[id] as guide %}
    {% if guide %}
        {% include "_guide_contents.tpl" guide=guide id=id %}
    {% else %}

    {% if id.o.haspart|is_visible as haspart %}
        <div class="content-list">
            {% for id in haspart %}
                {% catinclude "_list_item.tpl" id %}
            {% endfor %}
        </div>
    {% endif %}

    {% for s in id.s.haspart|is_visible %}
        {% with s.o.haspart|is_visible as siblings %}
        {% for p in s.o.haspart %}
            {% if p == id %}
                <p class="page-haspart">
                    {% if siblings[forloop.counter - 1] as prev %}
                        <a class="haspart__prev" href="{{ prev.page_url }}">{{ prev.title }}</a>
                    {% else %}
                        <span></span>
                    {% endif %}
                    <a class="haspart__link" href="{{ s.page_url }}">{{ s.title }}</a>
                    {% if siblings[forloop.counter + 1] as next %}
                        <a class="haspart__next" href="{{ next.page_url }}">{{ next.title }}</a>
                    {% endif %}
                </p>
            {% endif %}
        {% endfor %}
        {% endwith %}
    {% endfor %}

    {% endif %}
    {% endwith %}

    {% include "_page_documentation_connections.tpl" id=id %}

    {% if id.s.references  as refs %}
        <div class="connections">
            <h3>{_ Referred by _}</h3>
            <div class="list-items">
                {% for id in refs %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}
</div>
{% endblock %}

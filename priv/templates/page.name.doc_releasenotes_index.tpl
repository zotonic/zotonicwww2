{% extends "page.tpl" %}

{% block content_after %}
<div class="page-relations">

    {% with id.o.haspart
            |is_visible
            |zotonicwww2_by_version
            |first as newest_release
    %}
        {% if newest_release %}
            <p class="release-index__label">{_ Newest version _}</p>
            <div class="content-list content-list--releases">
                {% catinclude "_list_item.tpl" newest_release label=_"Release notes" %}
            </div>
        {% endif %}

        {% with m.search.query::%{
                cat: [ "releasenotes" ],
                is_published: true,
                sort: [ "-publication_start", "-id" ],
                pagelen: 1000,
                page: 1
            } as releases_by_date
        %}
            {% if releases_by_date %}
                <p class="release-index__label">{_ Releases by date _}</p>
                <div class="content-list content-list--releases">
                    {% for release_id in releases_by_date|without:newest_release %}
                        {% catinclude "_list_item.tpl" release_id %}
                    {% endfor %}
                </div>
            {% endif %}
        {% endwith %}
    {% endwith %}

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

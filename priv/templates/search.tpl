{% extends "base.tpl" %}

{% block title %}
    {_ Search _}: {{ q.qs|escape }}
{% endblock %}

{% block body_class %}search{% endblock %}

{% block content %}
    <h1>
        <span class="text-muted">{_ Search _}</span> {% if q.qs %}“{{ q.qs|escape }}”{% endif %}
    </h1>

    <div class="search-results">
        {% with m.search.paged[
                {query text=q.qs
                       cat=[`text`, `video`, `document`, `category`]
                       cat_exclude=[ `template`, `releasenotes` ]
                       page=q.page
                       pagelen=20
                }
            ] as result %}

            <div class="connections paged">
                <div class="page-count">
                    {{ result.total }} <span>{_ Pages _}</span>
                </div>

                <div class="list-items">
                    {% for id in result %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>

            {% pager result=result qargs %}
        {% endwith %}
    </div>
{% endblock %}
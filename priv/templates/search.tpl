{% extends "base.tpl" %}

{% block title %}
    {_ Search _}: {{ q.qs|escape }}
{% endblock %}

{% block body_class %}search{% endblock %}

{% block content %}
    <h1>
        <span class="text-muted">{_ Search _}</span> {% if q.qs %}“{{ q.qs|escape }}”{% endif %}
    </h1>

    {% if not q.page %}
        {% if m.zotonicwww2_search.exact_match[q.qs] as match_ids %}
            <div class="search-results">
                <div class="connections paged">
                    <div class="page-count">
                        <span>{_ Exact match _}</span>
                    </div>
                    <div class="list-items">
                        {% for id in match_ids %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endfor %}
                    </div>
                </div>
            </div>
        {% endif %}

        {% if m.zotonicwww2_search.title_match[q.qs] as match_ids %}
            <div class="search-results">
                <div class="connections paged">
                    <div class="page-count">
                        <span>{_ Title match _}</span>
                    </div>
                    <div class="list-items">
                        {% for id in match_ids %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endfor %}
                    </div>
                </div>
            </div>
        {% endif %}
    {% endif %}

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
{% extends "base.tpl" %}

{% block title %}
    {_ Search _}: {{ q.qs|escape }}
{% endblock %}

{% block body_class %}search{% endblock %}

{% block content %}
    <h1>{_ Search _}</h1>

    <form class="search-form" action="{% url search %}" method="GET">
        <div class="label-floating">
            <input placeholder="{_ Text to search _}" name="qs" class="form-control" name="qs" autofocus value="{{ q.qs|escape }}">
        </div>
    </form>

    {% if q.qs|trim|length > 0 %}
        {% if not q.page and q.qs|trim|length > 0 %}
            {% if m.zotonicwww2_search.exact_match[q.qs] as match_ids %}
                <div class="search-results">
                    <div class="connections paged">
                        <h2>{_ Exact match _}</h2>
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
                        <h2>{_ Title match _}</h2>
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
                           cat=[`text`, `video`, `document`]
                           cat_exclude=[ `template`, `releasenotes` ]
                           page=q.page
                           pagelen=20
                    }
                ] as result %}

                <div class="connections paged">
                    <h2>
                        {{ result.total }} {_ Pages _}
                    </h2>

                    <div class="list-items">
                        {% for id in result %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endfor %}
                    </div>
                </div>

                {% pager result=result qargs %}
            {% endwith %}
        </div>
    {% endif %}
{% endblock %}
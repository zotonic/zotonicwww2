{% with q.page|default:1 as qpage %}
    {% if q.qs|trim|length > 0 %}
        {% if qpage == 1 and q.qs|trim|length > 0 %}
            {% if m.search.query::%{
                    cat: [ "text", "media" ],
                    cat_exclude: [ "dispatch" ],
                    "pivot.title": q.qs|trim
                } as result
            %}
                <div class="search-results">
                    <div class="connections paged">
                        <h2>{_ Exact match _}</h2>
                        <div class="list-items">
                            {% for id in result %}
                                {% catinclude "_list_item.tpl" id is_show_cat %}
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
                                {% catinclude "_list_item.tpl" id is_show_cat %}
                            {% endfor %}
                        </div>
                    </div>
                </div>
            {% endif %}
        {% endif %}

        <div class="search-results">
            {% with m.search.paged.query::%{
                        text: q.qs,
                        cat: [ "text", "media" ],
                        cat_exclude: [ "template", "releasenotes", "dispatch" ],
                        page: q.page,
                        pagelen: 20
                    }
                    as result %}

                <div class="connections paged">
                    <h2>{{ result.total }} {_ Pages _}</h2>

                    <div class="list-items">
                        {% for id in result %}
                            {% catinclude "_list_item.tpl" id is_show_cat %}
                        {% endfor %}
                    </div>
                </div>

                {% pager result=result qargs %}
            {% endwith %}
        </div>
    {% endif %}
{% endwith %}

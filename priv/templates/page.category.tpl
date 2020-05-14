{% extends "page.documentation.tpl" %}

{% block content_after %}

<div class="page-relations">

    {% if id.o.relation as relo %}
        <div class="connections">
            <h3>{_ See more _} &rarr;</h3>

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

    {% with m.search.paged[{query cat=id sort="pivot_title" pagelen=100 page=q.page}] as result %}
        <div class="connections paged" id="content-pager">
            <div class="page-count">
                {{ result.total }} <span>{_ Pages _}</span>
            </div>
            <div class="list-items">
                {% for id in result %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>

            {% pager result=result id=id qargs hide_single_page %}
        </div>
    {% endwith %}
</div>

{% endblock %}

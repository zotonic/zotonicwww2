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

    {% if id.o.haspart as haspart %}
        {# Only show the Table of Contents if the sub-pages are documentation
         # pages that are not in the current category.
         # Examples are the notifications with sub-pages where notifications
         # are combined per kind.
         #}
        {% if haspart[1].category_id != id %}
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
    {% endif %}

    {% with m.category[id].is_a.documentation
            | if : "pivot_title"
                 : "-created" as sort
    %}
        {% with m.search.paged[{query cat=id sort=sort pagelen=100 page=q.page}] as result %}
            <div class="connections paged" id="content-pager">
                <div class="page-count">
                    {{ result.total }} <span> {{ id.title|lower }} </span>
                </div>
                <div class="list-items">
                    {% for id in result %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>

                {% pager result=result id=id qargs hide_single_page %}
            </div>
        {% endwith %}
    {% endwith %}
</div>

{% endblock %}

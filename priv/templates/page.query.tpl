{#
    A query page is a page where in the admin a search query
    can be defined.

    This makes it possible to let an editor define searches to be
    displayed on their own pages.
#}
{% extends "page.tpl" %}

{% block content_after %}

<div class="page-relations">
    {% with m.search.paged[{query query_id=id sort="-created" pagelen=100 page=q.page}] as result %}
        <div class="connections paged" id="content-pager">
            <p class="page-count">
                {{ result.total }} <span> {{ id.title|lower }} </span>
            </p>
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

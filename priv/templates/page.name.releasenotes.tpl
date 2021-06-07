{% extends "page.category.tpl" %}

{% block content_after %}

<div class="page-relations">

    {% with m.search.paged[{query cat=id sort=sort pagelen=1000 page=q.page}] as result %}
        <div class="connections paged" id="content-pager">
            <div class="page-count">
                {{ result.total }} <span> {{ id.title|lower }} </span>
            </div>
            <div class="list-items">
                {% for id in result|make_list|zotonicwww2_by_version %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>

            {% pager result=result id=id qargs hide_single_page %}
        </div>
    {% endwith %}
</div>

{% endblock %}

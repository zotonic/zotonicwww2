{#
    This is a page for the page with name 'website_example'
    This page is a category, for which normally 'page.category.tpl' would be used.
    As the name is more specific, this template 'page.name.website_example.tpl'
    is selected.
#}
{% extends "page.tpl" %}

{% block content_after %}

<div class="page-relations">
    {% with m.search.paged[{featured cat=id page=q.page}] as result %}
        <div class="connections paged" id="content-pager">
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
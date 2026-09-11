{% extends "page.documentation.tpl" %}

{% block content_after %}
    <div class="page-relations reference-category-page cookbook-page">
        <div id="{{ #cookbook_explorer }}">
            {% include "_cookbook_category_explorer.tpl"
                category_id=id
                is_new_query
            %}
        </div>
        {% live
            topic="model/location/event/qlist"
            template="_cookbook_category_explorer.tpl"
            target=#cookbook_explorer
            method="patch"
            category_id=id
            is_new_query
        %}
    </div>
{% endblock %}

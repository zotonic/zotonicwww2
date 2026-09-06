{% extends "page.documentation.tpl" %}

{# Shared by the named reference-category pages. Keeping this separate from
 # page.category.tpl preserves the dedicated keyword taxonomy pages. #}
{% block content_after %}
    <div class="page-relations reference-category-page">
        {% block reference_category_before %}{% endblock %}
        <div id="{{ #category_explorer }}"></div>
        {% live
            topic="model/location/event/qlist"
            template="_reference_category_explorer.tpl"
            target=#category_explorer
            method="patch"
            category_id=id
            is_new_query
        %}
    </div>
{% endblock %}

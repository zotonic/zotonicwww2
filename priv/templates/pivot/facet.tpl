{% overrules %}

{#
    Important documentation text. The `_ft` suffix makes mod_search store a
    normalized value and create a PostgreSQL GIN pg_trgm index for it.
#}
{% block important_ft %}
    {{ id.title }}
    {{ id.summary|truncatechars:300 }}
    {% for subject_id in id.o.subject %}
        {{ subject_id.title }}
    {% endfor %}
{% endblock %}

{# Facets used by the public documentation search. #}
{% block category_id %}{{ id.category_id }}{% endblock %}
{% block subject_ids %}{{ id.o.subject|join:"||" }}{% endblock %}
{% block module_ids %}{{ id.o.in_module|join:"||" }}{% endblock %}

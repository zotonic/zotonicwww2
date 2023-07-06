{% extends "page.category.tpl" %}

{# This page is used for the resource with unique name "releasenotes", which is the   #}
{# category of all release notes. Instead of displaying all release notes we redirect #}
{# to the documentation page which lists all release notes in descending order.       #}

{% block content %}

    <h2>{_ Redirecting... _}</h2>

    {# Upon visit, redirect to the the page with unique name "doc_releasenotes_index" #}
    {% wire action={redirect id="doc_releasenotes_index"} %}

{% endblock %}

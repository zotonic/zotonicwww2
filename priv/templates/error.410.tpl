{% extends "error.404.tpl" %}

{% block title %}{_ Page gone _} — {{ m.site.title }}{% endblock %}

{% block body_class %}error-page error-page--410{% endblock %}

{% block error_code %}410{% endblock %}

{% block error_heading %}{_ This page is no longer available _}{% endblock %}

{% block error_message %}{_ We used the words in its former address to look for useful documentation. You can refine the search below. _}{% endblock %}

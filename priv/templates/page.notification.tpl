{% extends "page.documentation.tpl" %}

{% block content_after %}
    {% inherit %}

    {% if id.s.observes|is_visible as modules %}
        <div class="page-relations">
            <section class="connections" id="notification-observed-by">
                <h3>{_ Observed by _}</h3>

                <div class="list-items">
                    {% for module_id in modules %}
                        {% catinclude "_list_item.tpl" module_id %}
                    {% endfor %}
                </div>
            </section>
        </div>
    {% endif %}
{% endblock %}

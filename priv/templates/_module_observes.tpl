{% if module_id.o.observes|is_visible as notifications %}
    <section class="connections" id="module-observes">
        <h3>{_ Observes _}</h3>

        <div class="list-items">
            {% for notification_id in notifications %}
                {% catinclude "_list_item.tpl" notification_id %}
            {% endfor %}
        </div>
    </section>
{% endif %}

{% if m.rsc[module_id].is_external_module %}
    {% if m.rsc[module_id].doc_module_observers as observers %}
        <section class="connections" id="module-observes">
            <h3>{_ Observes _}</h3>
            <div class="list-items">
                {% for observer in observers %}
                    {% with m.rsc[observer.page_name].id as notification_id %}
                        {% if notification_id.is_a.notification and notification_id.is_visible %}
                            {% catinclude "_list_item.tpl" notification_id %}
                        {% else %}
                            <article class="content-list__item">
                                <p class="content-list__meta"><span>{_ Notification _}</span></p>
                                <div class="content-list__copy">
                                    <h3 class="content-list__title">{{ observer.name }}</h3>
                                </div>
                            </article>
                        {% endif %}
                    {% endwith %}
                {% endfor %}
            </div>
        </section>
    {% endif %}
{% else %}
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
{% endif %}

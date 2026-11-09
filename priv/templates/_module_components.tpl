
{% for category_id in [
    `notification`,
    `model`,
    `cotonic_model`,
    `controller`,
    `dispatch`,
    `template_tag`,
    `template_filter`,
    `template_action`,
    `template_scomp`,
    `template_validator`
] %}
    {% if m.search[{query cat=category_id hasobject=[module_id, `in_module`] sort=`pivot_title`}] as result %}
        {% if is_navigation %}
            <li>
                <a class="module-components-nav__link" href="#module-components-{{ category_id.name }}">
                    {% if category_id.name == "notification" %}{_ Defined notifications _}{% else %}{{ category_id.title }}{% endif %} <span>{{ result|length }}</span>
                </a>
            </li>
        {% else %}
            <section class="connections" id="module-components-{{ category_id.name }}">
                <h3>{% if category_id.name == "notification" %}{_ Defined notifications _}{% else %}{{ category_id.title }}{% endif %}</h3>

                <div class="list-items">
                    {% for component_id in result %}
                        {% catinclude "_list_item.tpl" component_id %}
                    {% endfor %}
                </div>
            </section>
        {% endif %}
    {% endif %}
{% endfor %}

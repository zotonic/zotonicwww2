
{% for category_id in [
    `model`,
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
                    {{ category_id.title }} <span>{{ result|length }}</span>
                </a>
            </li>
        {% else %}
            <section class="connections" id="module-components-{{ category_id.name }}">
                <h3>{{ category_id.title }}</h3>

                <div class="list-items">
                    {% for component_id in result %}
                        {% catinclude "_list_item.tpl" component_id %}
                    {% endfor %}
                </div>
            </section>
        {% endif %}
    {% endif %}
{% endfor %}

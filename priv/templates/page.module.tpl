{% extends "page.documentation.tpl" %}

{% block content_before_body %}
    <nav class="module-components-nav" aria-label="{_ Module contents _}">
        <ul>
            {% if id.o.observes|is_visible as notifications %}
                <li>
                    <a class="module-components-nav__link" href="#module-observes">
                        {_ Notifications _} <span>{{ notifications|length }}</span>
                    </a>
                </li>
            {% endif %}
            {% include "_module_components.tpl" module_id=id is_navigation %}
        </ul>
    </nav>
{% endblock %}

{% block content_after %}

<div class="page-relations">
    {% include "_module_observes.tpl" module_id=id %}

    {% include "_module_components.tpl" module_id=id %}

    {% with id.o.relation|is_visible as relo %}
    {% with id.s.relation|is_visible as rels %}
    {% with id.s.haspart|is_visible -- [id.category_id] as hasparts %}
        {% if relo or rels or hasparts %}
            <div class="connections">
                <h3>{_ See also _}</h3>

                <div class="list-items">
                    {% for id in hasparts %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                    {% for id in relo %}
                        {% if not id|member:hasparts %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                    {% for id in rels %}
                        {% if not id|member:hasparts and not id|member:relo %}
                            {% catinclude "_list_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}
    {% endwith %}
    {% endwith %}
    {% endwith %}

    {% if id.s.references|is_visible as refs %}
        <div class="connections">
            <h3>{_ Referred by _}</h3>
            <div class="list-items">
                {% for rid in refs %}
                    {% catinclude "_list_item.tpl" rid %}
                {% endfor %}
            </div>
        </div>
    {% endif %}

{#
    <div class="connections">
        <h3>&#8712; {{ id.category_id.title }} <span class="text-muted">{_ Category _}</span></h3>

        <div class="list-items">
            {% catinclude "_list_item.tpl" id.category_id %}
        </div>
    </div>
#}
</div>

{% endblock %}

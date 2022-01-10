{% extends "page.documentation.tpl" %}

{% block content_after %}

<div class="page-relations">

    {% for cat in [
        `model`,
        `controller`,
        `dispatch`,
        `template_tag`,
        `template_filter`,
        `template_action`,
        `template_scomp`,
        `template_validator`
    ] %}

        {% if m.search[{query cat=cat hasobject=[id, `in_module`] sort=`pivot_title`}] as result %}
            <div class="connections">
                <h3>{{ cat.title }}</h3>

                <div class="list-items">
                    {% for id in result %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}
    {% endfor %}

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

{# Sidedrawer with main navigation
 # The main navigation is populated from the 'menu' resource
 # with the name 'main_menu'.
 #
 # We walk the menu tree in such a way that the current
 # levels on the path are shown.
 #
 # The variable `id` is set to the currently active menu item.
 #}

<label id="nav-label2" for="nav-toggle">&times;</label>

{#
<form class="search-form" method="get" action="{% url search %}">
    <input type="text" name="qs" class="form-control" placeholder="{_ Search _}" value="{{ q.qs|escape }}">
</form>
#}

<ul class="nav nav-stacked">

    {# Show the 'main_menu'
     # The top level menu has divider divs between the items.
     # The current page (variable "id") receives the class "selected".
     # The submenu is shown for all navigation items on the path to the
     # current page and below the current page.
     #}
    {% with m.rsc.main_menu.id|menu_expand as menu %}
        {% with [
                    id,
                    id.category_id,
                    id.s.haspart[1],
                    is.s.haspart[1].category_id
                ]|menu_trail:menu as trail %}
            {% with trail|last as id %}
                {% for menu in menu %}
                    {% if menu.id.is_visible %}
                        <li class="{% if menu.id == id %}selected {% endif %} {% if menu.id|member:trail %} nav__show{% endif %}">
                            <a href="{{ menu.id.page_url }}">{{ menu.id.short_title|default:menu.id.title }}</a>
                            {% if menu.tree %}
                                {% include "_nav_sidedrawer_submenu.tpl" menu=menu.tree %}
                            {% endif %}
                        </li>
                    {% endif %}
                {% endfor %}
            {% endwith %}
        {% endwith %}
    {% endwith %}

    {% if m.acl.user %}
        <li class="divider"></li>
        {% if id.is_editable %}
        <li>
            <a href="{% url admin_edit_rsc id=id %}"><span class="fa fa-pencil"></span> {_ Edit page _}</a>
        </li>
        {% endif %}
        <li>
            <a href="{% url admin_edit_rsc id=`main_menu` %}"><span class="fa fa-pencil"></span> {_ Edit menu _}</a>
        </li>
        <li>
            <a href="{% url logoff %}"><span class="fa fa-sign-out"></span> {_ Log off _}</a>
        </li>
    {% endif %}
</ul>

{# Sidedrawer with main navigation
 # The main navigation is populated from the 'menu' resource
 # with the name 'main_menu'.
 #
 # We walk the menu tree in such a way that the current
 # levels on the path are shown.
 #
 # The variable `id` is set to the currently active menu item.
 #}

<div class="logo if-full-width">
    <a href="{% url home %}">
        <img src="/lib/images/zotonic/zotonic-logo.png" alt="{{ m.site.title }}" class="img-responsive">
    </a>
</div>

<form class="search-form" method="get" action="{% url search %}">
    <input type="text" name="qs" class="form-control" placeholder="{_ Search _}" value="{{ q.qs|escape }}">
</form>

<ul class="nav nav-stacked">

    {# Show the 'main_menu'
     # The top level menu has divider divs between the items.
     # The current page (variable "id") receives the class "selected".
     # The submenu is shown for all navigation items on the path to the
     # current page and below the current page.
     #}
    {% if id.category_id|menu_trail as trail %}
        {% with id.category_id as id %}
            {% for mid, submenu in m.rsc.main_menu.menu %}
                <li {% if mid == id %}class="selected"{% endif %}>
                    <a href="{{ mid.page_url }}">{{ mid.short_title|default:mid.title }}</a>
                    {% if submenu and mid|member:trail %}
                        {% include "_nav_sidedrawer_submenu.tpl" menu=submenu %}
                    {% endif %}
                </li>
            {% endfor %}
        {% endwith %}
    {% else %}
        {% with id|menu_trail as trail %}
            {% for mid, submenu in m.rsc.main_menu.menu %}
                <li {% if mid == id %}class="selected"{% endif %}>
                    <a href="{{ mid.page_url }}">{{ mid.short_title|default:mid.title }}</a>
                    {% if submenu and mid|member:trail %}
                        {% include "_nav_sidedrawer_submenu.tpl" menu=submenu %}
                    {% endif %}
                </li>
            {% endfor %}
        {% endwith %}
    {% endif %}

    {% if m.acl.user %}
        <li class="divider"></li>
        <li>
            <ul class="nav nav-stacked">
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
            </ul>
        </li>
    {% endif %}
</ul>

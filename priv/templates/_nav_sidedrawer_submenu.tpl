<ul class="nav nav-stacked">
{% for mid, submenu in menu %}
    <li {% if mid == id %}class="selected"{% endif %}>
        <a href="{{ mid.page_url }}">{{ mid.short_title|default:mid.title }}</a>

        {# Shows all menu items on the path to the current menu item, and the
         # submenu below the current menu item.
         #}
        {% if submenu and mid|member:trail %}
            {% include "_nav_sidedrawer_submenu.tpl" menu=submenu %}
        {% endif %}
    </li>
{% endfor %}
</ul>

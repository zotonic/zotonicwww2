{# This is the navigation at the top of the page.
 # It shows the logo and the "hamburger" menu to toggle the sidebar
 #}

<nav class="navbar navbar-grid {% if not m.acl.user %}no-user{% endif %}">
  <label id="nav-label" for="nav-toggle">☰</label>

  <!-- Brand logo or name -->
  <div class="home-link">
      <a href="{% url home %}">
        <img src="{% image_url '/lib/images/zotonic/zotonic-logo.png' mediaclass='logo' %}" alt="{{ m.site.title }}">
      </a>
  </div>

  <div class="nav-wrapper">
    <div class="nav-wrapper2">

      <ul class="nav">
        {% with [
                    id,
                    id.category_id,
                    id.s.haspart[1],
                    is.s.haspart[1].category_id
                ]|menu_trail as trail %}
            {% with trail|last as id %}
                {% for menu in m.rsc.main_menu.menu %}
                  <li {% if menu.id|member:trail %}class="selected"{% endif %}>
                      <a href="{{ menu.id.page_url }}">{{ menu.id.short_title|default:menu.id.title }}</a>
    {% comment %}
                        {% if menu.tree and menu.id|member:trail %}
                            {% include "_nav_sidedrawer_submenu.tpl" menu=menu.tree %}
                        {% endif %}
    {% endcomment %}
                  </li>
                {% endfor %}
            {% endwith %}
        {% endwith %}
      </ul>
    </div>
  </div>
</nav>

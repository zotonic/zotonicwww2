{# This is the navigation at the top of the page.
 # It shows the logo and the "hamburger" menu to toggle the sidebar
 # navigation on smaller screens.
 #}

<nav class="navbar navbar-grid {% if not m.acl.user %}no-user{% endif %}">
  <label id="nav-label" for="nav-toggle">☰</label>

  <!-- Brand logo or name -->
  <div class="home-link">
      <a href="{% url home %}">
        <img src="{% image_url '/lib/images/zotonic/zotonic-logo.png' mediaclass='logo' %}" alt="{{ m.site.title }}">
      </a>
  </div>
</nav>

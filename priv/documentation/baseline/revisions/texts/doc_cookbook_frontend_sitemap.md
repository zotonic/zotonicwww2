# Create a public site-map page

Create a published text page in admin. Give it the unique name `page_site_map` and page path `/site-map`; ensure visitors can see it. Add `priv/templates/page.name.page_site_map.tpl` to your site:

```django
{% extends "page.tpl" %}
{% block content %}
    <h1>{{ id.title }}</h1>
    {% menu id=main_menu %}
{% endblock %}
```

Use the content block defined by your site's actual page layout if it differs. `main_menu` must be the unique name of an existing menu resource; connect the pages you want listed in that menu. Enable `mod_menu` when needed.

Visit `/site-map` as a logged-out visitor. Check every link, including pages hidden by access rules. This lists your menu structure, not every database resource. For a crawler-oriented XML sitemap, use `module#mod_seo_sitemap` instead of exposing all resources in an HTML loop.

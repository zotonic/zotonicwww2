---
name: "developer_first_dynamic_page"
title: "Add your first dynamic page"
summary: "Use an existing controller and a template for a simple dynamic page."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 6
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["tutorial", "frontend_developer", "template", "dispatch_rule", "routing_and_redirects"]
---

# Add your first dynamic page

Use an existing controller and a template for a simple dynamic page.

For a new dispatch file, use the complete list below. In an existing dispatch list, insert the tuple before broader matching rules and separate entries with commas:

```erlang
[
    {garden_welcome, ["welcome"], controller_template,
        [{template, "garden_welcome.tpl"}]}
].
```

Create `priv/templates/garden_welcome.tpl`:

```django
{% extends "base.tpl" %}
{% block content %}
    <h1>{_ Welcome _}</h1>
    <p>{{ m.site.title|escape }}</p>
{% endblock %}
```

Confirm that your generated `base.tpl` has a `content` block; adapt the block name if it differs. Reload `/welcome` on the site's configured hostname and check that the site title appears.

Use `{% url garden_welcome %}` to link to this route. For ordinary CMS content, prefer resource page URLs instead of inventing a dispatch rule per page. See [Follow a request to its template](../04-templates/request-path.md) and [Link to resources and dispatch routes](../04-templates/resource-links.md).

Save the dispatch list as `apps_user/garden/priv/dispatch/garden` (create the directory if needed). Save the template under `apps_user/garden/priv/templates/`. After file handling has refreshed the rules, run `bin/zotonic dispatch garden /welcome`; expect the `garden_welcome` rule and `controller_template` before checking the browser.

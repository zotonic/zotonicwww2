---
name: "developer_first_template_change"
title: "Change a template and see the result"
summary: "Make a small visible change before building a larger feature."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_getting_started"
order: 5
required_modules: []
source_paths: ["rebar.config", "GNUmakefile", "apps/zotonic_mod_zotonic_site_management/priv/skel", "apps/zotonic_launcher/src/command/zotonic_cmd_addsite.erl"]
zotonic_keywords: ["tutorial", "frontend_developer", "template", "render", "development_and_debugging"]
---

# Change a template and see the result

With [the garden blog running](create-site.md), change its home page. For the `blog` skeleton used in this walkthrough, the file is `apps_user/garden/priv/templates/home.tpl`.

1. Open that file in your editor on your computer.
2. Find `{% block content %}` and add this paragraph immediately below it, before the existing content:

```django
<p>{_ Welcome to the community garden. _}</p>
```

3. Save the file. In a container setup, the edited checkout is already mounted into the container.
4. Reload [the home page](https://garden.test:8443/). Expect **Welcome to the community garden.** above the blog content.

You have now built Zotonic, created a site, and changed a rendered page. Try changing the sentence once more to see the edit–reload cycle.

If the change does not appear, check that you edited `apps_user/garden`, not `_build`, and that the browser uses the garden hostname. Check [file watching](../03-command-line-workflows/automatic-rebuild.md) and the development logs for template errors. Sites created with a different skeleton may use another template; use [template selection](../04-templates/template-selection.md) to find it.

Continue with [Add your first dynamic page](first-dynamic-page.md) when you want to create a new route.

---
name: "developer_request_path"
title: "Follow a request to its template"
summary: "Start with the URL that gives an unexpected result. Check its hostname: Zotonic uses it to select a site before matching the site's dispatch rules."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 1
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["explanation", "frontend_developer", "template", "render"]
---

# Follow a request to its template

Start with the URL that gives an unexpected result. Check its hostname: Zotonic uses it to select a site before matching the site's dispatch rules.

Run `bin/zotonic dispatch garden /welcome`. Read the selected dispatch name, controller, and arguments. A template controller renders its configured template. A page controller also works with a content resource, often available as `id` in the template.

Next, inspect the active template selected for that name. Includes and category variants can select additional files from other active modules. Use [Find the template used by a page](template-selection.md) to find those files. Test the actual browser request too: dispatch inspection does not reproduce every authentication, language, or response condition.

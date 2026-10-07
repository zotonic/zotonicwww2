---
name: "developer_template_selection"
title: "Find the template used by a page"
summary: "Enable module#mod_development and open its template tools in the admin. Search for the template name and inspect the selected file and alternatives."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 2
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "render"]
---

# Find the template used by a page

Enable `module#mod_development` and open its template tools in the admin. Search for the template name and inspect the selected file and alternatives.

If two active modules provide the same path, module priority decides which one is selected. A file in an inactive module does not override the current template. Confirm the site context before changing priorities.

Use the template trace while loading the page to follow the templates that are actually rendered. Category includes and conditional branches mean that a static list of files is not enough to explain every request.

---
name: "developer_template_xref"
title: "Check template references"
summary: "Open Cross-reference check of templates in the Development page and run the check. Read each reported reference with the active module set in mind."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 6
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "template", "development_and_debugging"]
---

# Check template references

Open **Cross-reference check of templates** in the Development page and run the check. Read each reported reference with the active module set in mind.

Fix a misspelled template name or missing static include at its source. If a reference is supplied dynamically, verify the possible values rather than replacing it solely to silence a report.

Run the check again after the change, then render a page that exercises the affected branch. Reference checking helps find missing dependencies, but it does not test permissions, content values, or the final HTML.

---
name: "developer_testing_templates"
title: "Check a rendered template"
summary: "Render the changed page with realistic content. Include missing optional fields, a long title, several list items, and a second language when applicable."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_testing"
order: 3
required_modules: []
source_paths: ["apps/zotonic_core/test", "apps/zotonic_launcher/src/command/zotonic_cmd_runtests.erl", "apps/zotonic_core/src/support/z_sitetest.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "development_and_debugging", "validate"]
---

# Check a rendered template

Render the changed page with realistic content. Include missing optional fields, a long title, several list items, and a second language when applicable.

Check links, image descriptions, heading order, and keyboard access. For category-specific markup, test a resource in the exact category and one that uses the fallback template.

Run the template cross-reference check for missing dependencies. Then check the browser console and network responses for scripts or styles that did not load. Save a screenshot when the visual result matters to review.

---
name: "developer_enable_development"
title: "Enable the Development module"
summary: "Sign in to the local site's admin as an administrator. Open the module management page, find Development (mod_development), and activate it if necessary."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 2
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging"]
---

# Enable the Development module

Sign in to the local site's admin as an administrator. Open the module management page, find Development (`mod_development`), and activate it if necessary.

Open **System → Development**. The page groups settings and links for templates, dispatch, observers, and function tracing. Availability can depend on the site's environment, permissions, and supporting modules.

Enable only the diagnostic setting needed for the current task. Several settings affect the whole site, while database tracing is tied to the current session. Leave the unauthenticated development API disabled for normal browser and command-line work.

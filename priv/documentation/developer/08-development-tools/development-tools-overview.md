---
name: "developer_development_tools_overview"
title: "Choose a development tool"
summary: "Start with the symptom and use the smallest tool that explains it."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_development_tools"
order: 1
required_modules: []
source_paths: ["apps/zotonic_mod_development/src/mod_development.erl", "apps/zotonic_mod_development/src/models/m_development.erl", "apps/zotonic_mod_development/priv/templates", "apps/zotonic_filehandler/src"]
zotonic_keywords: ["explanation", "backend_developer", "development_and_debugging"]
---

# Choose a development tool

Start with the symptom and use the smallest tool that explains it.

| Symptom | Tool |
| --- | --- |
| Wrong markup | Template selection and live template trace |
| Unexpected URL handling | Dispatch list and path trace |
| Slow page | Database trace, logs, then focused function trace |
| Observer does not run | Observer list and module status |
| Saved changes are missing | Build output, file handling, template selection |
| Result changes after a flush | Cache keys and invalidation |

![Site Development screen with template settings, live reload, tracing, and template debugging tools.](../assets/development-tools.jpg)

Open System → Development to find these controls. The checkboxes shown are the test site’s settings; enable only the tools needed for your current investigation.

Enable the Development module on your local site. Keep the ordinary browser developer tools open for network and JavaScript problems. See [Enable the Development module](enable-development.md), [Trace templates rendered by a request](template-trace.md), and [Trace a URL through dispatch](dispatch-debugging.md).

---
name: "developer_running_sites"
title: "Check running sites"
summary: "Run:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 3
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Check running sites

Run:

```sh
bin/zotonic status
```

The output identifies the node and lists site states. Locate `garden` rather than treating “Running” for the node as sufficient.

If the site is stopped, inspect its enabled setting and startup logs. If the site is not listed, check its application directory, configuration file, build output, and discovery. A hostname error can also send browser requests to another site even when the desired site runs correctly.

Use `bin/zotonic sitedir garden` to identify the application's location on a running node. Follow with a real request to the site after changing startup configuration.

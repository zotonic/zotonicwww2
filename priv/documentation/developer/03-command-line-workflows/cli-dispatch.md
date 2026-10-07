---
name: "developer_cli_dispatch"
title: "Trace URL dispatch from the command line"
summary: "Use dispatch inspection to discover why a path selects a particular controller:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 10
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "dispatch_rule", "routing_and_redirects"]
---

# Trace URL dispatch from the command line

Use dispatch inspection to discover why a path selects a particular controller:

```sh
bin/zotonic dispatch garden
bin/zotonic dispatch garden detail
bin/zotonic dispatch garden /welcome
```

The first form lists site rules, the second includes detail, and the third traces a path. You can also pass a full URL to include hostname-based site selection.

Compare the selected rule, controller, options, and any path rewriting with your expectation. Earlier applicable rules can win over a broad catch-all later in the list.

This is a dispatch diagnosis, not a substitute for making the HTTP request and checking authorization and rendering. See [Trace a URL through dispatch](../08-development-tools/dispatch-debugging.md) and [A URL reaches the wrong page](../11-troubleshooting/wrong-route.md).

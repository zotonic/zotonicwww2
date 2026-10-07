---
name: "developer_site_lifecycle"
title: "Start, stop, and restart a site"
summary: "Use these commands for one named site:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 4
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Start, stop, and restart a site

Use these commands for one named site:

```sh
bin/zotonic startsite garden
bin/zotonic restartsite garden
bin/zotonic stopsite garden
```

They are alternatives for different tasks; do not run the whole block as a setup recipe. The node must be running. A restart can interrupt requests and site-specific workers, so use it deliberately.

After starting or restarting, inspect `status`, the logs, and a page on the site's configured hostname. Starting a site does not replace missing dependencies or fix invalid configuration.

Use a normal code reload for a small Erlang edit when appropriate. Reserve whole-node `stop` and `restart` for changes that need that broader scope. See [Compile changed code and load modules](compile-load.md).

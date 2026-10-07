---
name: "developer_startup_modes"
title: "Start in the foreground or background"
summary: "Choose a startup mode for the environment:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 2
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Start in the foreground or background

Choose a startup mode for the environment:

```sh
bin/zotonic debug
bin/zotonic start_nodaemon
bin/zotonic start
```

Run one of these, not all three. `debug` keeps an interactive Erlang shell in the foreground. `start_nodaemon` runs in the foreground without that shell, which suits a process supervisor. `start` runs in the background.

Use `bin/zotonic status` afterwards to inspect the node and sites. To attach to an existing instance, use `shell`.

`stop` stops the Erlang VM and every site on it. `restart` restarts Zotonic and its sites within the VM. Use site-specific commands for a change that should affect only one site. See [Start, stop, and restart a site](site-lifecycle.md).

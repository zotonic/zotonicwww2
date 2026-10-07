---
name: "developer_compile_load"
title: "Compile changed code and load modules"
summary: "Use make for the normal project build. On a running development node, the CLI also provides focused operations:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 7
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Compile changed code and load modules

Use `make` for the normal project build. On a running development node, the CLI also provides focused operations:

```sh
bin/zotonic compile
bin/zotonic compilefile apps_user/garden/src/garden.erl
bin/zotonic load
```

`compile` asks the running file handler to compile modified Erlang sources. `compilefile` targets a file. `load` invokes the changed-BEAM loading pass; it does not take an individual module selector in the current implementation.

Read compiler output and logs before retesting. A command returning from an asynchronous request does not prove the entire rebuild has finished.

Use `update` when you need the broader compile/load/cache/index refresh workflow. Do not rebuild or flush everything repeatedly when a source error explains the failure. See [Automatic recompilation and file watching](automatic-rebuild.md).

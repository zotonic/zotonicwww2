---
name: "developer_shell_connect"
title: "Connect to the running Erlang shell"
summary: "From the checkout root:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 5
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Connect to the running Erlang shell

From the checkout root:

```sh
bin/zotonic shell
```

This attaches to the live node. Expressions run against that node's actual code and data, so start with read-only inspection.

```erlang
C = z:c(garden).
z_context:site(C).
```

Expect the site name `garden`. A fresh site context is not automatically an administrator context. Use an authenticated request context or deliberate, local-only elevation when testing mutations.

Leave the remote shell with **Ctrl-C twice**. Do not call `q().` or `halt().` to detach: they can stop the shared node.

If connection fails, check the target node name, Erlang distribution setup, and cookie configuration. See [Work with a site context](../05-content-data/site-context.md) and [Inspect content from the Erlang shell](shell-data.md).

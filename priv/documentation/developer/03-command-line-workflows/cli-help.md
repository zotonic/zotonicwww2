---
name: "developer_cli_help"
title: "Find help and available commands"
summary: "Run bin/zotonic from the checkout root to see the available commands. Consult this guide's Command-line reference collection for their arguments and effects."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 1
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Find help and available commands

Run `bin/zotonic` from the checkout root to see the available commands. Consult this guide's [Command-line reference](../12-command-reference/index.md) collection for their arguments and effects.

Use task-specific commands instead of guessing flags from other tools. For example, `dispatch` accepts a site name or URL, while `rpc` passes trailing arguments as strings rather than parsing Erlang expressions.

Before running a command, identify its scope: the entire node, one site, a local file, or an external browser. Configuration commands can display secrets; inspect output locally and redact it before sharing.

The launcher and the server must agree on their node configuration and authentication. A connection error is not proof that a site's code failed to compile. See [Connect to the running Erlang shell](shell-connect.md).

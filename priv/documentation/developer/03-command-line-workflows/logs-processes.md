---
name: "developer_logs_processes"
title: "Inspect logs and running processes"
summary: "Use bin/zotonic logtail for the configured console log, or select error or crash when those files are available. The command prints the last 500 lines and exits; it does not follow the log continuously."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 11
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "logging_and_monitoring", "erlang_otp"]
---

# Inspect logs and running processes

Use `bin/zotonic logtail` for the configured console log, or select `error` or `crash` when those files are available. The command prints the last 500 lines and exits; it does not follow the log continuously.

For live process activity, use `bin/zotonic etop` if the Erlang installation includes the required tool. Identify the process and time period responsible for repeated activity before changing configuration.

Prefer structured logs containing the operation, result, reason on failure, and a useful resource or site identifier. Avoid logging request bodies or tokens as a shortcut to understanding a problem.

A busy process alone does not identify the slow request. Correlate request timing with template, function, and database traces. See [Write logs that explain an operation](../08-development-tools/structured-logging.md) and [A page is slow](../11-troubleshooting/slow-requests.md).

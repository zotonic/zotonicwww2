---
name: "developer_cli_tests"
title: "Run tests and site checks"
summary: "Use the project's test workflow in a dedicated development or test environment. Two CLI entry points have different scopes:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 13
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Run tests and site checks

Use the project's test workflow in a dedicated development or test environment. Two CLI entry points have different scopes:

- `runtests` starts the configured test runner and accepts test selections.
- `sitetest garden` stops the site, drops and uses the `z_sitetest` database schema, runs site tests, then restarts with normal configuration. Use a disposable test environment and avoid concurrent runs sharing the database.

Read the relevant reference page before choosing arguments. Tests may create or change data; a passing startup check is not a complete test of models, permissions, and browser behavior.

Run a focused test for the change, then the repository's required broader checks. Preserve the first meaningful error and reproduce it with a minimal case rather than repeatedly restarting the whole node.

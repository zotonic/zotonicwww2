---
name: "developer_database_shell"
title: "Test the database connection"
summary: "Run bin/zotonic connectdb to test a connection using the global Zotonic database configuration. It does not open PostgreSQL’s interactive client and has no site selector in this checkout."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 12
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "database", "postgresql", "query"]
---

# Test the database connection

Run `bin/zotonic connectdb` to test a connection using the global Zotonic database configuration. It does not open PostgreSQL’s interactive client and has no site selector in this checkout.

The command prints connection options, including the password, before testing. Keep that output private. A site can override global database settings, so success does not prove that its own database and schema are available.

For a site-specific investigation, inspect its configuration and use a database client deliberately, or query application data through Zotonic models with the correct context. Prefer models for normal mutations so notifications and cache invalidation remain coordinated.

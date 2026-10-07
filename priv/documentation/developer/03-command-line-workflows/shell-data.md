---
name: "developer_shell_data"
title: "Inspect content from the Erlang shell"
summary: "With C = z:c(garden) in an attached shell:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 6
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "development_and_debugging", "erlang_otp"]
---

# Inspect content from the Erlang shell

With `C = z:c(garden)` in an attached shell:

```erlang
Id = m_rsc:rid(<<"page_home">>, C).
m_rsc:p(Id, title, C).
m_edge:objects(Id, haspart, C).
```

The named resource must exist on that site; `undefined` can mean the chosen name is absent. Use the generated site's real resource names rather than assuming every skeleton creates identical content.

Inspect individual properties first instead of dumping an entire resource containing private fields. Use models for normal content access, and understand the ACL context before interpreting missing results.

For a portable representation, inspect `model#rsc_export`. For changes, use `model#rsc` and `model#edge`, then check the public result separately. See [Use stable resource names](../05-content-data/resource-identifiers.md).

---
name: "developer_cli_translations"
title: "Extract translation messages"
summary: "After changing translatable template text in a site, generate its message catalog:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_line_workflows"
order: 14
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command", "apps/zotonic_core/src/support/z.erl"]
zotonic_keywords: ["how_to_guide", "backend_developer", "localization_and_translation"]
---

# Extract translation messages

::: aside
Extracting messages does not translate content resources stored in the database.
:::

After changing translatable template text in a site, generate its message catalog:

```sh
bin/zotonic pot garden
```

This connects to the running node and writes translation source files. Use `pot zotonic` only when intentionally updating the core catalogs according to the project's contribution workflow.

Review the resulting POT changes, merge them into the appropriate PO files, and translate new messages. Validate a PO file with gettext tooling:

```sh
msgfmt --check --output-file=/dev/null path/to/language.po
```

Check both the interface text and the selected content language in the browser. See [Translate interface text](../04-templates/template-translations.md).

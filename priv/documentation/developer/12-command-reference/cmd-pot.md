---
name: "developer_cmd_pot"
title: "pot: Generate gettext translation template files"
summary: "Generate gettext translation template files."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_command_reference"
order: 20
required_modules: []
source_paths: ["apps/zotonic_launcher/src/command/zotonic_cmd_pot.erl"]
zotonic_keywords: ["reference", "backend_developer", "localization_and_translation"]
---

# pot: Generate gettext translation template files

Generate gettext translation template files.

## Syntax

```text
bin/zotonic pot zotonic|<site_name>
```

## Requirements and effects

Requires a running node, the relevant translation support, and gettext command-line tools.

Pass a site name for that site or `zotonic` for the core modules. The command writes POT files; it is not a read-only report. Run it as part of translation work, review the changes, and merge the template into PO files separately.

## Example

```sh
bin/zotonic pot garden
```

Run commands from the Zotonic workspace root. Replace `garden` with your site name where applicable.

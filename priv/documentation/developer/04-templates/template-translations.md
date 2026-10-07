---
name: "developer_template_translations"
title: "Translate interface text"
summary: "Write the source text in English and mark static interface text for translation."
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 9
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "localization_and_translation", "translated_text"]
---

# Translate interface text

::: aside
Resource content translations are separate from interface strings.
:::

Write the source text in English and mark static interface text for translation.

```django
<button type="submit">{_ Save changes _}</button>
```

Use a translated argument when passing text into an include, for example `title=_"Latest news"`.

Generate the site's POT file when you are updating translations, then merge it into the PO files and translate the new messages. Validate changed PO files with `msgfmt --check`. Test a second language in the browser, including links and form errors.

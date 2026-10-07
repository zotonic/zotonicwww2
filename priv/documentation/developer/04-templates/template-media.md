---
name: "developer_template_media"
title: "Display images with mediaclasses"
summary: "Define reusable image sizes in priv/templates/mediaclass.config and use them in image tags. For example, a local mediaclass can constrain an image's width:"
category: "developerguide"
language: "en"
is_published: true
parent: "developer_collection_templates"
order: 8
required_modules: []
source_paths: ["doc/template-tags", "apps/zotonic_mod_base/priv/templates", "apps/zotonic_core/src/support/z_dispatcher.erl"]
zotonic_keywords: ["how_to_guide", "frontend_developer", "template", "media_management", "render"]
---

# Display images with mediaclasses

Define reusable image sizes in `priv/templates/mediaclass.config` and use them in image tags. For example, a local mediaclass can constrain an image's width:

```erlang
[
    {garden_card, [{width, 640}]}
].
```

```django
{% image id mediaclass="garden_card" alt=id.title %}
```

Choose a meaningful description for an image when its purpose is not captured by the title. Check both portrait and landscape source images. Cropping is a design decision; do not add it automatically to every size.

Use `model#media` for media information and `tag#image` for rendering. See [Work with uploaded media](../05-content-data/media-data.md) and [Build and serve frontend assets](asset-build.md).

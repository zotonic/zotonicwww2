# Render and manage media

A media item is a resource with a medium record. Use its resource ID or unique name in templates; do not build a public URL from the archive filename. The image tag selects and generates the requested preview:

```django
{% image id.depiction mediaclass="thumb" alt=id.title %}
```

Define the class in the site's `priv/templates/mediaclass.config`:

```erlang
[
    {"thumb", [{width, 200}, {height, 200}, {crop, center}]},
    {"masthead", [
        {width, 1600}, {quality, 85},
        {srcset, [{"640w", []}, {"1200w", []}, {"1600w", []}]},
        {sizes, "100vw"}
    ]}
].
```

`thumb` is a 200 by 200 cropped image. `masthead` uses width descriptors consistently. Do not mix `w` descriptors with `1x`/`2x` density descriptors in a single `srcset`. Adjust `sizes` to the actual rendered layout, not merely the source width.

After changing the configuration, let development discovery reload it or refresh the module index. Render `{% image id.depiction mediaclass="masthead" %}` and inspect the generated image, `srcset`, and network requests at different viewport widths. Test the original's dimensions and access permissions when a preview is missing.

On the server, `m_media:get(Id, Context)` returns metadata or `undefined`. Use `m_media:insert_file/3` or `m_media:replace_file/3` with the current context and handle their results; check their source contracts when supplying resource or medium properties. Importing a resource that points at remote media does not mean its asynchronous download has completed.

Keep the archive and database together in backups. Media generation depends on external tools, storage permissions, and the media sandbox. Diagnose the failing stage rather than disabling the sandbox for the whole installation.

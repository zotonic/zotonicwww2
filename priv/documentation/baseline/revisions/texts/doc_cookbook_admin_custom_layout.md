# Adjust the resource editor layout

The resource editor uses `tag#catinclude` for `_admin_edit_main_parts.tpl` and `_admin_edit_sidebar_parts.tpl`. A category-specific override can therefore change one resource type without changing every editor screen.

1. Find the category's unique name and inspect the installed admin templates.
2. Add a matching override under the site's `priv/templates`, for example `_admin_edit_main_parts.article.tpl`.
3. Start from the current main-parts template and make the smallest change. Prefer the `_admin_edit_content_extra.tpl` extension point when you only need an extra widget.
4. Preserve the parent resource form, hidden resource ID, save wiring, translation controls and permission checks.
5. Test saving, validation errors, language switching and read-only access for that category and another category.

Do not copy an old complete `admin_edit.tpl` merely to move one widget. Such copies easily miss new publishing, access or form behaviour after an upgrade.

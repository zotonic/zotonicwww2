# Use maintainable Zotonic patterns

Build a small working site first, then add one capability at a time. Keep site-specific configuration in the site and reusable behaviour in a module. Use stable resource names in fixtures and import routines.

Keep template components small, pass values explicitly, and use resource/category selection instead of duplicating dispatch rules. Escape non-resource values in the correct output context. Render media through the image/media pipeline and named mediaclasses.

The connected tasks cover site creation, datamodel fixtures, template composition and media rendering. Test changes with the intended user's permissions and keep source separate from runtime files.

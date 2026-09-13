The deletion cleaned up the category as intended: the cookbook API now returns 45 entries, with none of the old collection pages.

Most important findings:

- Unpublish or replace the empty placeholders Create a custom action and Writing your own module.
- Retire or completely rewrite the two “Just enough Erlang/OTP and rebar” articles: part 1 and part 2. They teach rebar 2-era commands while referring to rebar3.
- Create a custom model has malformed Erlang, incorrect m_get/3 return values, and an OMDb example that cannot work without an API key.
- Create a custom tag uses the removed gen_scomp behaviour and old Nitrogen-era implementation details. Current code uses [zotonic_scomp.erl](/Users/marc/Sites/zotonic-master/apps/zotonic_core/src/behaviours/zotonic_scomp.erl).
- Custom search has invalid Erlang syntax and demonstrates the old tuple-style search interface. The current observer receives #search_query{name, args, offsetlimit} in [mod_search.erl (line 234)](/Users/marc/Sites/zotonic-master/apps/zotonic_mod_search/src/mod_search.erl:234).
- Execute tasks asynchronously describes an API that still exists, but its example contains missing commas, a missing -module, and an Id/RscId variable error.
- Create a custom controller remains conceptually correct, but its combined export declaration has an invalid trailing comma.
- Customizing the sign-up and sign-in form lists numerous templates that no longer exist. It needs a rewrite based on the current mod_signup templates.
- Customizing the admin edit layout recommends replacing the main layout with a very incomplete version. That would now remove address, media, depiction, blocks, SEO and publishing controls. It should teach the category-specific catinclude extension points instead.
- Automatically add new users to a user group tests m_rsc:is_a/3 during the pre-persist notification. For a new resource that information is not yet stored; it should inspect the proposed properties or use #rsc_update_done{post_is_a=...}.
- Just enough Postgres contains PostgreSQL 10, Debian Lenny and obsolete Zotonic database-management instructions. This is better replaced with a much shorter current operational guide.
- Just enough Erlang shell demonstrates Erlang R13 and now(). It should be rewritten around current OTP and bin/zotonic shell.

Smaller corrections:

- Storing date/time fields names the removed _admin_edit_date.tpl; the current partial is _edit_date.tpl.
- Debugging DB query issues says logstatement; PostgreSQL’s setting is log_statement.
- Updating a form field from a dialog has a duplicated href attribute.
- Custom pivots should use ?MODULE, current search-map syntax, and current status/index-rebuild instructions.
- Logging to Logstash is mostly current, but says to replace “logger” when it means the configured host and uses an older logging call.
- Retrieving the category of a page no longer retrieves anything from the URL; the title and introduction are misleading.

The following are still fundamentally sound, though several need link and presentation cleanup: Pivot Templates, Growl Notifications, Icons in templates, custom filters, overriding Zotonic, custom error pages, the contact form, page blocks, site maps, module activation, admin widgets, and the XSS article. For example, the pivot blocks described by the cookbook still correspond to [pivot.tpl (line 11)](/Users/marc/Sites/zotonic-master/apps/zotonic_mod_base/priv/templates/pivot/pivot.tpl:11).
There is also widespread imported-RST residue: docutils markup, old /id/doc_* links, “See also” fragments, plain HTTP links, and references to very old external documentation. That can be handled as a separate bulk editorial cleanup after the broken recipes are unpublished or repaired.


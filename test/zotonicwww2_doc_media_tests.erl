-module(zotonicwww2_doc_media_tests).
-include_lib("eunit/include/eunit.hrl").

relative_image_test() ->
    Source = <<"apps/zotonic_mod_development/src/mod_development.erl">>,
    ?assertEqual({ok, <<"apps/zotonic_mod_development/priv/doc/admin-development.jpg">>},
        zotonicwww2_doc_media:relative_path(Source, <<"../priv/doc/admin-development.jpg">>)),
    ?assertError(outside_repository,
        zotonicwww2_doc_media:relative_path(Source, <<"../../../../secret.jpg">>)),
    ?assertEqual(remote, zotonicwww2_doc_media:relative_path(Source, <<"https://example.com/image.jpg">>)),
    ?assertEqual(remote, zotonicwww2_doc_media:relative_path(Source, <<"/lib/images/image.jpg">>)),
    ?assertError({badmatch, false},
        zotonicwww2_doc_media:relative_path(Source, <<"../doc/settings.config">>)).

missing_image_fails_test() ->
    Entry = #{body => <<"<img src=\"missing.png\" alt=\"Missing\">">>,
        source_path => <<"src/example.erl">>},
    ?assertMatch({error, {documentation_image, _, _}},
        zotonicwww2_doc_media:prepare(Entry, "/nonexistent-documentation-checkout", undefined)).

component_image_test() ->
    ?assertEqual({ok, <<"apps/zotonic_mod_base/priv/doc/scomp-base-debug-expanded.png">>},
        zotonicwww2_doc_media:relative_path(
            <<"apps/zotonic_mod_base/src/scomps/scomp_base_debug.erl">>,
            <<"../../priv/doc/scomp-base-debug-expanded.png">>)).

no_images_preserves_entry_test() ->
    Entry = #{body => <<"<p>Text without images.</p>">>, source_path => <<"src/example.erl">>},
    ?assertEqual({ok, Entry}, zotonicwww2_doc_media:prepare(Entry, "/unused", undefined)).

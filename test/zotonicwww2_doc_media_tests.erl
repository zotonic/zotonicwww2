-module(zotonicwww2_doc_media_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

media_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Root) -> [
        ?_test(relative_image(Root)),
        ?_test(component_image()),
        ?_test(invalid_relative_images()),
        ?_test(remote_images_preserved()),
        ?_test(missing_image_fails()),
        ?_test(no_images_preserves_entry())
    ] end}.

setup() ->
    Tmp = case os:getenv("TMPDIR") of false -> "/tmp"; Dir -> Dir end,
    Root = filename:join(Tmp, "zotonicwww2-doc-media-" ++
        integer_to_list(erlang:unique_integer([positive, monotonic]))),
    ok = file:make_dir(Root),
    lists:foreach(fun(Relative) ->
        File = filename:join(Root, Relative),
        ok = filelib:ensure_dir(File),
        ok = file:write_file(File, <<"test image contents">>)
    end, ["apps/zotonic_mod_development/priv/doc/admin-development.jpg",
          "apps/zotonic_mod_base/priv/doc/scomp-base-debug-expanded.png"]),
    meck:new(m_zotonicwww2_git, [no_link]),
    meck:expect(m_zotonicwww2_git, git_dir, fun(#context{}) -> Root end),
    meck:new(m_rsc, [no_link]),
    meck:expect(m_rsc, rid, fun(_, #context{}) -> undefined end),
    meck:new(m_media, [no_link]),
    meck:expect(m_media, insert_file, fun(_, _, #context{}) -> {ok, 123} end),
    Root.

cleanup(Root) ->
    meck:unload([m_zotonicwww2_git, m_rsc, m_media]),
    ok = file:del_dir_r(Root).

entry(Source, Url) ->
    #{source => <<"zotonic">>, source_path => Source,
      body => <<"<img src=\"", Url/binary, "\" alt=\"Example\">">>}.

relative_image(Root) ->
    Relative = <<"apps/zotonic_mod_development/priv/doc/admin-development.jpg">>,
    Entry = entry(<<"apps/zotonic_mod_development/src/mod_development.erl">>,
        <<"../priv/doc/admin-development.jpg">>),
    {ok, Result} = zotonicwww2_doc_media:prepare(Entry, #context{}),
    ?assertMatch(#{Relative := _}, maps:get(media_hashes, Result)),
    ?assertMatch(<<"<!-- z-media 123 ", _/binary>>, maps:get(body, Result)),
    ?assert(meck:called(m_media, insert_file, [filename:join(Root, Relative), '_', '_'])).

component_image() ->
    Relative = <<"apps/zotonic_mod_base/priv/doc/scomp-base-debug-expanded.png">>,
    Entry = entry(<<"apps/zotonic_mod_base/src/scomps/scomp_base_debug.erl">>,
        <<"../../priv/doc/scomp-base-debug-expanded.png">>),
    {ok, Result} = zotonicwww2_doc_media:prepare(Entry, #context{}),
    ?assertMatch(#{Relative := _}, maps:get(media_hashes, Result)).

invalid_relative_images() ->
    Source = <<"apps/zotonic_mod_development/src/mod_development.erl">>,
    ?assertEqual({error, {documentation_image, Source, outside_repository}},
        zotonicwww2_doc_media:prepare(entry(Source, <<"../../../../secret.jpg">>), #context{})),
    ?assertEqual({error, {documentation_image, Source, {badmatch, false}}},
        zotonicwww2_doc_media:prepare(entry(Source, <<"../doc/settings.config">>), #context{})).

remote_images_preserved() ->
    lists:foreach(fun(Url) ->
        Entry = entry(<<"src/example.erl">>, Url),
        ?assertEqual({ok, Entry}, zotonicwww2_doc_media:prepare(Entry, #context{}))
    end, [<<"https://example.com/image.jpg">>, <<"/lib/images/image.jpg">>]).

missing_image_fails() ->
    Source = <<"src/example.erl">>,
    Entry = entry(Source, <<"missing.png">>),
    ?assertEqual({error, {documentation_image, Source, {badmatch, {error, enoent}}}},
        zotonicwww2_doc_media:prepare(Entry, #context{})).

no_images_preserves_entry() ->
    Entry = #{source => <<"zotonic">>, source_path => <<"src/example.erl">>,
        body => <<"<p>Text without images.</p>">>},
    ?assertEqual({ok, Entry}, zotonicwww2_doc_media:prepare(Entry, #context{})).

-module(zotonicwww2_doc_media_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

media_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Root) -> [
        ?_test(relative_image(Root)),
        ?_test(component_image()),
        ?_test(external_local_image(Root)),
        ?_test(external_remote_image(Root)),
        ?_test(external_remote_restrictions(Root)),
        ?_test(external_download_failure(Root)),
        ?_test(invalid_relative_images()),
        ?_test(diagram_sources_rejected()),
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

diagram_sources_rejected() ->
    Source = <<"doc/guides/storage.md">>,
    lists:foreach(fun(Name) ->
        Entry = entry(Source, <<"images/", Name/binary>>),
        ?assertEqual({error, {documentation_image, Source, {badmatch, false}}},
            zotonicwww2_doc_media:prepare(Entry, #context{}))
    end, [<<"flow.dot">>, <<"flow.DOT">>, <<"flow.md.dot">>,
          <<"flow.mmd">>, <<"flow.MMD">>, <<"flow.md.mmd">>]).

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

external_local_image(Root) ->
    Relative = <<"apps/zotonic_mod_base/priv/doc/scomp-base-debug-expanded.png">>,
    Entry = (entry(<<"README.md">>, Relative))#{source => <<"external_42">>},
    {ok, Result} = zotonicwww2_doc_media:prepare_external(Entry, Root, #context{}),
    ?assertMatch(<<"<!-- z-media 123 ", _/binary>>, maps:get(body, Result)),
    ?assert(meck:called(m_media, insert_file, [filename:join(Root, Relative),
        meck:is(fun(Props) -> maps:get(<<"doc_image_source">>, Props) =:=
            <<"external_42:", Relative/binary>> end), '_'])).

external_remote_image(Root) ->
    Url = <<"https://user-images.githubusercontent.com/123/diagram.png">>,
    File = filename:join(Root, "download.png"),
    meck:expect(m_media, download_file, fun(U, Options, #context{}) ->
        ?assertEqual(Url, U),
        ?assertEqual(false, proplists:get_value(autoredirect,
            proplists:get_value(fetch_options, Options))),
        ?assertEqual(none, proplists:get_value(authorization,
            proplists:get_value(fetch_options, Options))),
        ok = file:write_file(File, <<"downloaded image">>),
        {ok, File, "diagram.png"}
    end),
    Entry = (entry(<<"README.md">>, Url))#{source => <<"external_42">>},
    {ok, Result} = zotonicwww2_doc_media:prepare_external(Entry, Root, #context{}),
    ?assertMatch(<<"<!-- z-media 123 ", _/binary>>, maps:get(body, Result)),
    ?assertMatch(#{Url := _}, maps:get(media_hashes, Result)),
    ?assertNot(filelib:is_file(File)),
    %% Reimport uses the existing media when its content hash is unchanged.
    Hash = maps:get(Url, maps:get(media_hashes, Result)),
    meck:expect(m_rsc, rid, fun(_, _) -> 123 end),
    meck:expect(m_rsc, p, fun
        (123, doc_image_source, _) -> <<"external_42:", Url/binary>>;
        (123, doc_image_hash, _) -> Hash
    end),
    meck:expect(m_media, get, fun(123, _) -> #{} end),
    meck:reset(m_media),
    ?assertEqual({ok, Result}, zotonicwww2_doc_media:prepare_external(Entry, Root, #context{})),
    ?assertNot(meck:called(m_media, insert_file, ['_', '_', '_'])),
    meck:expect(m_rsc, rid, fun(_, _) -> undefined end).

external_remote_restrictions(Root) ->
    meck:reset(m_media),
    lists:foreach(fun(Url) ->
        Entry = (entry(<<"README.md">>, Url))#{source => <<"external_42">>},
        ?assertEqual({ok, Entry}, zotonicwww2_doc_media:prepare_external(Entry, Root, #context{}))
    end, [<<"https://localhost/private.png">>,
        <<"https://user-images.githubusercontent.com.evil.example/image.png">>,
        <<"https://user-images.githubusercontent.com:8443/image.png">>,
        <<"https://user@user-images.githubusercontent.com/image.png">>,
        <<"http://user-images.githubusercontent.com/image.png">>,
        <<"https://user-images.githubusercontent.com/image.svg">>]),
    ?assertNot(meck:called(m_media, download_file, ['_', '_', '_'])).

external_download_failure(Root) ->
    meck:expect(m_media, download_file, fun(_, _, _) -> {error, timeout} end),
    Entry = (entry(<<"README.md">>,
        <<"https://raw.githubusercontent.com/example/repo/main/image.png">>))#{source => <<"external_42">>},
    ?assertMatch({error, {documentation_image, <<"README.md">>, _}},
        zotonicwww2_doc_media:prepare_external(Entry, Root, #context{})).

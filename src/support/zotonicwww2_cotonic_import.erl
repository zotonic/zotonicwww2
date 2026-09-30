%% @doc Import Cotonic's HTML reference as separate reference and model pages.
%% The upstream index is hand-maintained HTML. Token boundaries preserve its
%% examples despite imperfect nesting. IDs identify pages and operation anchors;
%% links are resolved against the complete manifest before anything is stored.
-module(zotonicwww2_cotonic_import).

-export([import_docs/1, poll/1, collect_entries/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Called by the serialized documentation task queue after admin authorization.
-spec import_docs(Context) -> Result when
    Context :: z:context(), Result :: {ok, map()} | {error, term()}.
import_docs(Context) ->
    import_docs(true, Context).

%% @doc Fetch daily and import only when the upstream revision has changed.
-spec poll(Context) -> Result when
    Context :: z:context(), Result :: {ok, map()} | {error, term()}.
poll(Context) ->
    import_docs(false, Context).

import_docs(IsForce, Context) ->
    Imported = m_config:get_value(zotonicwww2, cotonic_imported_hash, Context),
    Dir = filename:join(filename:dirname(m_zotonicwww2_git:git_dir(Context)), "cotonic-git"),
    case checkout(Dir) of
        {ok, Imported} when not IsForce ->
            {ok, #{}};
        {ok, Commit} ->
            {ok, Html} = file:read_file(filename:join(Dir, "index.html")),
            Entries = collect_entries(Html),
            case zotonicwww2_subject_import:import(Context) of
                {ok, _} ->
                    case zotonicwww2_doc_import:sync(<<"cotonic">>, Entries, Commit, Context) of
                        {ok, Report} ->
                            m_config:set_value(zotonicwww2, cotonic_imported_hash, Commit, Context),
                            {ok, Report};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

checkout(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, ".keep")),
    Commands = case filelib:is_dir(filename:join(Dir, ".git")) of
        true -> ["git fetch --depth=1 origin +refs/heads/master:refs/remotes/origin/master",
                 "git checkout --detach --force origin/master", "git rev-parse HEAD"];
        false -> ["git clone --depth=1 --branch master https://github.com/cotonic/cotonic.git .",
                  "git rev-parse HEAD"]
    end,
    lists:foldl(fun
        (_, {error, _} = Error) -> Error;
        (Command, {ok, _}) ->
            case exec:run(Command, [sync, stdout, stderr, {cd, Dir}]) of
                {ok, Output} ->
                    {ok, z_string:trim(iolist_to_binary(proplists:get_value(stdout, Output, [])))};
                {error, _} = Error -> Error
            end
    end, {ok, <<>>}, Commands).

%% @doc Pure collector, also usable for validating an upstream checkout offline.
%% Refuse incomplete or unexpectedly restructured sources before reconciliation.
-spec collect_entries(Html) -> Entries when
    Html :: binary(), Entries :: [map()].
collect_entries(Html) ->
    Tokens = z_html_parse:tokens(Html),
    [_ | Rest] = lists:dropwhile(fun
        ({start_tag, <<"section">>, Attrs, _}) ->
            proplists:get_value(<<"id">>, Attrs) =/= <<"documentation">>;
        (_) -> true
    end, Tokens),
    Content = lists:takewhile(fun(T) -> T =/= {end_tag, <<"section">>} end, Rest),
    Sections = split_sections(Content, <<"introduction">>, [], []),
    Ids = [ Id || {Id, _} <- Sections ],
    Names = [page_name(Id) || Id <- Ids],
    true = length(Names) =:= length(lists:usort(Names)),
    true = lists:all(fun(Id) -> lists:member(Id, Ids) end,
        [<<"workers">>, <<"functions">>, <<"models">>, <<"worker.functions">>
         | [<<"model.", M/binary>> || M <- required_model_names()]]),
    Anchors = lists:foldl(fun({Id, Ts}, Acc) ->
        lists:foldl(fun
            ({start_tag, _, Attrs, _}, A) ->
                case proplists:get_value(<<"id">>, Attrs) of
                    undefined -> A;
                    Anchor -> A#{Anchor => page_name(Id)}
                end;
            (_, A) -> A
        end, Acc#{Id => page_name(Id)}, Ts)
    end, #{}, Sections),
    [entry(Id, Ts, Anchors, Sections) || {Id, Ts} <- Sections].

split_sections([], Id, Acc, Sections) ->
    lists:reverse([{Id, lists:reverse(Acc)} | Sections]);
split_sections([{start_tag, Tag, Attrs, _} = T | Rest], Id, Acc, Sections)
    when Tag =:= <<"h2">>; Tag =:= <<"h3">> ->
    case proplists:get_value(<<"id">>, Attrs) of
        undefined -> split_sections(Rest, Id, [T | Acc], Sections);
        Next -> split_sections(Rest, Next, [T], [{Id, lists:reverse(Acc)} | Sections])
    end;
split_sections([T | Rest], Id, Acc, Sections) ->
    split_sections(Rest, Id, [T | Acc], Sections).

entry(Id, Tokens, Anchors, Sections) ->
    Body = iolist_to_binary(z_html_parse:to_html(lists:flatmap(fun(T) -> rewrite(T, Anchors) end, section_headings(Id, Tokens)))),
    Index = case Id of
        <<"introduction">> -> index(Sections);
        <<"models">> -> index([{I, Ts} || {<<"model.", _/binary>> = I, Ts} <- Sections]);
        _ -> <<>>
    end,
    #{name => page_name(Id), title => title(Id),
      category => category(Id), kind => cotonic,
      body => operation_headings(<<Body/binary, Index/binary>>),
      source_path => <<"index.html">>,
      source_url => <<"https://github.com/cotonic/cotonic/blob/master/index.html">>,
      keywords => lists:usort([<<"cotonic">>, <<"javascript">>, <<"frontend_developer">>,
                              <<"reference">> | keywords(Id)]),
      props => #{<<"page_path">> => page_path(Id), <<"doc_source_anchor">> => Id}}.

%% The section heading becomes the resource title, regardless of its wording.
%% Preserve its source anchor, and promote remaining H3 subsections below the H1.
section_headings(_Id, []) -> [];
section_headings(Id, [{start_tag, Tag, Attrs, _} = Token | Rest])
    when Tag =:= <<"h1">>; Tag =:= <<"h2">>; Tag =:= <<"h3">> ->
    case proplists:get_value(<<"id">>, Attrs) of
        Id ->
            [{end_tag, Tag} | Tail] = lists:dropwhile(fun(T) -> T =/= {end_tag, Tag} end, Rest),
            [{start_tag, <<"a">>, [{<<"name">>, Id}], false}, {end_tag, <<"a">>}
                | section_headings(Id, Tail)];
        _ when Tag =:= <<"h3">> ->
            [{start_tag, <<"h2">>, Attrs, false} | section_headings(Id, Rest)];
        _ -> [Token | section_headings(Id, Rest)]
    end;
section_headings(Id, [{end_tag, <<"h3">>} | Rest]) ->
    [{end_tag, <<"h2">>} | section_headings(Id, Rest)];
section_headings(Id, [Token | Rest]) ->
    [Token | section_headings(Id, Rest)].

%% Promote upstream paragraph labels to H2 sections below the page title.
%% This also keeps operations at the top level of the standard TOC.
%% Keep any function signature with its heading and preserve the paragraph body.
operation_headings(Html) ->
    re:replace(Html,
        <<"<p(?:\\s[^>]*)?>\\s*<(strong|em) class=\"header\">((?:(?!</\\1>).)*)</\\1>"
          "(\\s*<code>(?:(?!</code>).)*</code>)?\\s*(?:<br\\s*/?>)?">>,
        <<"<h2>\\2\\3</h2><p>">>,
        [global, dotall, {return, binary}]).

index(Sections) ->
    iolist_to_binary([<<"<ul>">>, [
        [<<"<li><a href=\"/id/">>, page_name(Id), <<"\">">>,
         z_html:escape(title(Id)), <<"</a></li>">>]
        || {Id, _} <- Sections, Id =/= <<"introduction">>
    ], <<"</ul>">>]).

rewrite({start_tag, Tag, Attrs, Singleton}, Anchors) ->
    %% The resource sanitizer strips IDs, but retains named anchors. Keep an
    %% explicit anchor outside the element so malformed upstream paragraphs
    %% cannot swallow it during HTML repair.
    Anchor = case proplists:get_value(<<"id">>, Attrs) of
        undefined -> [];
        Id -> [{start_tag, <<"a">>, [{<<"name">>, Id}], false}, {end_tag, <<"a">>}]
    end,
    Anchor ++ [{start_tag, Tag, [rewrite_attr(A, Anchors) || A <- Attrs], Singleton}];
rewrite(T, _) -> [T].

rewrite_attr({<<"href">>, <<"#", Anchor/binary>>}, Anchors) ->
    case maps:find(Anchor, Anchors) of
        {ok, Name} -> {<<"href">>, <<"/id/", Name/binary, "#", Anchor/binary>>};
        error ->
            %% Preserve an upstream destination for pre-existing broken anchors.
            {<<"href">>, <<"https://cotonic.org/#", Anchor/binary>>}
    end;
rewrite_attr({Key, Value}, _) when Key =:= <<"href">>; Key =:= <<"src">> ->
    {Key, z_convert:to_binary(uri_string:resolve(Value, <<"https://cotonic.org/">>))};
rewrite_attr(Attr, _) -> Attr.

page_name(Id) -> <<"doc_cotonic_", (z_string:to_name(Id))/binary>>.

page_path(<<"introduction">>) -> <<"/cotonic">>;
page_path(<<"model.", Model/binary>>) ->
    <<"/cotonic/models/", (z_string:to_name(Model))/binary>>;
page_path(Id) -> <<"/cotonic/", (z_string:to_name(Id))/binary>>.

category(<<"model.", _/binary>>) -> cotonic_model;
category(_) -> cotonic_reference.

title(<<"introduction">>) -> <<"Cotonic">>;
title(<<"model.", Model/binary>>) -> <<"model/", Model/binary>>;
title(<<"workers">>) -> <<"Workers">>;
title(<<"functions">>) -> <<"Page functions">>;
title(<<"worker.functions">>) -> <<"Worker functions">>;
title(<<"models">>) -> <<"Models">>;
title(<<"links">>) -> <<"Links">>;
title(<<"changelog">>) -> <<"Change log">>;
title(Id) -> Id.

%% Baseline sections must remain present. Additional models are discovered
%% automatically, allowing imports before and after upstream documentation merges.
required_model_names() ->
    [<<"window">>, <<"document">>, <<"location">>, <<"ui">>, <<"lifecycle">>,
     <<"autofocus">>, <<"serviceWorker">>, <<"localStorage">>, <<"sessionStorage">>, <<"dedup">>].

%% Section IDs are the upstream HTML anchors; values are controlled keyword slugs.
keywords(Id) ->
    Keywords = #{
        <<"workers">> => [<<"web_workers">>],
        <<"worker.functions">> => [<<"web_workers">>, <<"publish_and_subscribe">>],
        <<"functions">> => [<<"web_workers">>],
        <<"cotonic.broker">> => [<<"message_broker">>, <<"publish_and_subscribe">>],
        <<"cotonic.mqtt_bridge">> => [<<"message_bridge">>, <<"mqtt">>],
        <<"cotonic.mqtt">> => [<<"mqtt">>, <<"publish_and_subscribe">>],
        <<"cotonic.ui">> => [<<"dom">>, <<"render">>],
        <<"cotonic.tokenizer">> => [<<"html">>, <<"parse">>],
        <<"model.window">> => [<<"model">>, <<"browser_navigation">>],
        <<"model.document">> => [<<"model">>, <<"dom">>, <<"user_interface_and_interaction">>],
        <<"model.location">> => [<<"model">>, <<"browser_navigation">>],
        <<"model.ui">> => [<<"model">>, <<"dom">>, <<"user_interface_and_interaction">>],
        <<"model.lifecycle">> => [<<"model">>, <<"page_lifecycle">>],
        <<"model.autofocus">> => [<<"model">>, <<"dom">>, <<"user_interface_and_interaction">>],
        <<"model.serviceWorker">> => [<<"model">>, <<"service_workers">>],
        <<"model.localStorage">> => [<<"model">>, <<"browser_storage">>],
        <<"model.sessionId">> => [<<"model">>, <<"browser_storage">>, <<"identifier">>],
        <<"model.sessionStorage">> => [<<"model">>, <<"browser_storage">>],
        <<"model.dedup">> => [<<"model">>, <<"messaging_and_pubsub">>]
    },
    Default = case Id of
        <<"model.", _/binary>> -> [<<"model">>, <<"messaging_and_pubsub">>];
        _ -> [<<"messaging_and_pubsub">>]
    end,
    maps:get(Id, Keywords, Default).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

operation_headings_test() ->
    Html = <<"<a name=\"operation\"></a><p>\n<strong class=\"header\">post/reload</strong>"
        "<br>Reload.</p><pre>&lt;strong class=\"header\"&gt;Example&lt;/strong&gt;</pre>">>,
    Converted = operation_headings(Html),
    ?assertEqual(<<"<a name=\"operation\"></a><h2>post/reload</h2><p>Reload.</p>"
        "<pre>&lt;strong class=\"header\"&gt;Example&lt;/strong&gt;</pre>">>, Converted),
    ?assertEqual(<<"<h2>call <code>call(topic)</code></h2><p>Call.</p>">>,
        operation_headings(<<"<p><strong class=\"header\">call</strong> "
            "<code>call(topic)</code><br>Call.</p>">>)),
    {ShortToc, _} = filter_toc:toc(Converted, 4, undefined),
    ?assertEqual([], ShortToc),
    {LongToc, Body} = filter_toc:toc(binary:copy(Converted, 4), 4, undefined),
    ?assertEqual(4, length(LongToc)),
    ?assert(lists:all(fun({_, _, Children}) -> Children =:= [] end, LongToc)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"post/reload</h2>">>)).

configuration_heading_test() ->
    ?assertEqual(<<"<a name=\"model.serviceWorker.config\"></a>"
        "<h2><em>Configuration</em></h2><p>Options.</p>">>,
        operation_headings(<<"<a name=\"model.serviceWorker.config\"></a><p>"
            "<strong class=\"header\"><em>Configuration</em></strong>"
            "<br>Options.</p>">>)),
    ?assertEqual(<<"<h2>Configuration</h2><p>Options.</p>">>,
        operation_headings(<<"<p><em class=\"header\">Configuration</em>"
            "<br>Options.</p>">>)).

missing_heading_break_test() ->
    ?assertEqual(<<"<h2>post/+key</h2><p>Store.</p><pre>example</pre>"
        "<h2>post/+key/+subkey</h2><p>Subkey.</p>">>,
        operation_headings(<<"<p><strong class=\"header\">post/+key</strong>"
            "Store.</p><pre>example</pre><p><strong class=\"header\">post/+key/+subkey</strong>"
            "<br>Subkey.</p>">>)).

section_headings_test() ->
    Tokens = z_html_parse:tokens(<<"<h3 id=\"cotonic.broker\">Broker</h3>"
        "<p>Introduction.</p><h3>Installation</h3><p>Install.</p>">>),
    Html = iolist_to_binary(z_html_parse:to_html(section_headings(<<"cotonic.broker">>, Tokens))),
    ?assertEqual(<<"<a name=\"cotonic.broker\"></a><p>Introduction.</p>"
        "<h2>Installation</h2><p>Install.</p>">>, Html).

split_and_link_test() ->
    Entries = collect_entries(test_document()),
    Models = [E || #{category := cotonic_model} = E <- Entries],
    ?assertEqual(10, length(Models)),
    [Location] = [E || #{name := <<"doc_cotonic_model_location">>} = E <- Entries],
    ?assert(lists:member(<<"browser_navigation">>, maps:get(keywords, Location))),
    ?assertEqual(<<"/cotonic/models/location">>, maps:get(<<"page_path">>, maps:get(props, Location))),
    [Intro] = [E || #{name := <<"doc_cotonic_introduction">>} = E <- Entries],
    Body = maps:get(body, Intro),
    ?assertNotEqual(nomatch, binary:match(Body, <<"/id/doc_cotonic_model_location#model.location.get.href">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"https://cotonic.org/doc/images/example.png">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"outside-script">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"sidebar-only">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"&amp;lt;">>)),
    Sanitized = z_html:sanitize(maps:get(body, Location)),
    ?assertNotEqual(nomatch, binary:match(Sanitized, <<"name=\"model.location.get.href\"">>)).

session_id_document_test() ->
    Html = binary:replace(test_document(), <<"<h3 id='model.dedup'>">>,
        <<"<h3 id='model.sessionId'>model/sessionId</h3>",
          "<p id='model.sessionId.get'>Read the identifier.</p>",
          "<a href='#model.sessionId.get'>Get</a>",
          "<h3 id='model.dedup'>">>),
    Entries = collect_entries(Html),
    ?assertEqual(11, length([E || #{category := cotonic_model} = E <- Entries])),
    [Session] = [E || #{name := <<"doc_cotonic_model_sessionid">>} = E <- Entries],
    ?assertEqual(<<"model/sessionId">>, maps:get(title, Session)),
    ?assertEqual(<<"/cotonic/models/sessionid">>,
        maps:get(<<"page_path">>, maps:get(props, Session))),
    ?assert(lists:member(<<"browser_storage">>, maps:get(keywords, Session))),
    ?assert(lists:member(<<"identifier">>, maps:get(keywords, Session))),
    Body = z_html:sanitize(maps:get(body, Session)),
    ?assertNotEqual(nomatch, binary:match(Body,
        <<"/id/doc_cotonic_model_sessionid#model.sessionId.get">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"model.sessionId.get\"">>)).

incomplete_document_test() ->
    ?assertException(error, _, collect_entries(<<"<section id='documentation'><p>Incomplete</p></section>">>)).

unknown_anchor_test() ->
    ?assertEqual({<<"href">>, <<"https://cotonic.org/#missing">>},
        rewrite_attr({<<"href">>, <<"#missing">>}, #{})).

test_document() ->
    iolist_to_binary([
        <<"<section id='sidebar'>sidebar-only</section><section id='documentation'>",
          "<p id='introduction'>Cotonic</p><a href='#model.location.get.href'>Location</a>",
          "<img src='doc/images/example.png'><pre>&lt;example&gt;</pre>">>,
        [ [<<"<h2 id='">>, Id, <<"'>Heading</h2><p>Documentation</p>">>]
          || Id <- [<<"workers">>, <<"functions">>, <<"models">>, <<"worker.functions">>] ],
        [ [<<"<h3 id='model.">>, M, <<"'>Model</h3><p id='model.">>, M,
           <<".get.href'>Operation</p>">>] || M <- required_model_names() ],
        <<"</section><script>outside-script</script>">>
    ]).
-endif.

%% @doc Import the reviewed editorial documentation bundle from priv/documentation.
%% Build import/plan.json before deployment. Audit is read-only; import requires
%% an administrator and an explicit matching site/environment. No implicit sudo.
-module(zotonicwww2_documentation_import).
-export([audit/2, import/2]).
-ifdef(TEST).
-export([integration_test/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("zotonic_core/include/zotonic.hrl").

-define(OWNER, <<"doc_editorial_bundle">>).
-define(EDGES, <<"doc_editorial_edges">>).
-define(MEDIA_HASH, <<"doc_editorial_media_hash">>).

-spec audit(Options, Context) -> {ok, map()} | {error, term()}
    when Options :: map(), Context :: z:context().
audit(Options, Context) ->
    guarded(fun() ->
        check_destination(Options, Context),
        {Dir, Plan} = load_plan(Context),
        {ok, preflight(Dir, Plan, Context)}
    end).

-spec import(Options, Context) -> {ok, map()} | {error, term()}
    when Options :: map(), Context :: z:context().
import(Options, Context) ->
    guarded(fun() ->
        check_destination(Options, Context),
        %% One importer per site, including distributed nodes sharing its database.
        global:trans({{?MODULE, z_context:site(Context)}, self()}, fun() ->
            {Dir, Plan} = load_plan(Context),
            Audit = preflight(Dir, Plan, Context),
            case maps:get(errors, Audit) of
                [] -> apply_plan(Dir, Plan, Context);
                Errors -> {error, {preflight, Errors}}
            end
        end)
    end).

check_destination(#{site := Site, environment := Environment}, Context) ->
    true = z_acl:is_admin(Context) orelse error(eacces),
    Site = z_context:site(Context),
    Environment = m_site:environment(Context),
    ok;
check_destination(_, _) ->
    error({required_options, [site, environment]}).

guarded(Fun) ->
    try Fun() catch
        Class:Reason:Stack ->
            ?LOG_ERROR(#{in => zotonicwww2, text => <<"Documentation import failed">>,
                result => error, reason => Reason, class => Class, stack => Stack}),
            {error, {Class, Reason}}
    end.

load_plan(Context) ->
    Dir = filename:join(code:priv_dir(zotonicwww2), "documentation"),
    Plan = read_json(filename:join([Dir, "import", "plan.json"])),
    <<"zotonic-documentation-import-v1">> = maps:get(<<"format">>, Plan),
    lists:foreach(fun(#{<<"file">> := File, <<"sha256">> := Hash}) ->
        true = Hash =:= file_hash(safe_file(Dir, File)) orelse error({stale_input, File})
    end, maps:get(<<"inputs">>, Plan)),
    TaxonomyHash = maps:get(<<"taxonomy_sha256">>, Plan),
    true = TaxonomyHash =:= file_hash(zotonicwww2_subject_import:csv_path(Context))
        orelse error(stale_keyword_taxonomy),
    Rows = all_rows(Plan),
    Names = [maps:get(<<"name">>, R) || R <- Rows],
    true = length(Names) =:= length(lists:usort(Names)),
    lists:foreach(fun(R) ->
        [] = maps:keys(maps:get(<<"properties">>, R)) -- [<<"title">>, <<"summary">>, <<"body">>],
        true = lists:all(fun is_binary/1, maps:values(maps:get(<<"properties">>, R)))
    end, Rows),
    {Dir, Plan}.

all_rows(Plan) -> maps:get(<<"resources">>, Plan) ++ maps:get(<<"media">>, Plan).

read_json(File) ->
    {ok, Bin} = file:read_file(File),
    z_json:decode(Bin).

safe_file(Dir, File) ->
    relative = filename:pathtype(File),
    false = lists:member(<<"..">>, filename:split(z_convert:to_binary(File))),
    filename:join(Dir, File).

file_hash(File) ->
    {ok, Bin} = file:read_file(File),
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

resolve(#{<<"name">> := Name, <<"source_uri">> := Uri}, Context) ->
    Named = m_rsc:rid(Name, Context),
    ByUri = case Uri of <<>> -> undefined; _ -> m_rsc:rid(Uri, Context) end,
    case {Named, ByUri} of
        {undefined, Id} -> Id;
        {Id, undefined} -> Id;
        {Id, Id} -> Id;
        Conflict -> error({identity_conflict, Name, Conflict})
    end.

preflight(_Dir, Plan, Context) ->
    Rows = all_rows(Plan),
    Ids = [{R, resolve(R, Context)} || R <- Rows],
    ExistingIds = [Id || {_, Id} <- Ids, is_integer(Id)],
    true = length(ExistingIds) =:= length(lists:usort(ExistingIds))
        orelse error(duplicate_destination_identity),
    Planned = [maps:get(<<"name">>, R) || R <- Rows] ++ maps:get(<<"keyword_names">>, Plan),
    Categories = lists:usort([maps:get(<<"category">>, R) || R <- Rows]),
    Predicates = lists:usort([maps:get(<<"predicate">>, E) || E <- maps:get(<<"edges">>, Plan)]),
    true = lists:all(fun(P) -> lists:member(P,
        [<<"haspart">>, <<"relation">>, <<"hasreference">>, <<"subject">>, <<"depiction">>]) end, Predicates),
    Required = Categories ++ Predicates ++ [<<"default_content_group">>],
    EdgeTargets = lists:flatmap(fun(E) -> [maps:get(<<"subject">>, E) | maps:get(<<"objects">>, E)] end,
        maps:get(<<"edges">>, Plan)),
    BodyTargets = lists:flatmap(fun(R) -> body_targets(maps:get(<<"body">>, maps:get(<<"properties">>, R), <<>>)) end, Rows),
    Missing = [{missing_resource, Name} || Name <- lists:usort(Required ++ EdgeTargets ++ BodyTargets),
        not lists:member(Name, Planned), m_rsc:rid(Name, Context) =:= undefined],
    Collisions = lists:flatmap(fun({R, Id}) -> collision(R, Id, Context) end, Ids),
    Paths = [{maps:get(<<"page_path">>, R, <<>>), Id, maps:get(<<"name">>, R)} || {R, Id} <- Ids],
    PathErrors = [{page_path_collision, Name, Path} || {Path, undefined, Name} <- Paths,
        is_binary(Path), Path =/= <<>>, is_integer(m_rsc:page_path_to_id(Path, Context))],
    #{site => z_context:site(Context), environment => m_site:environment(Context),
      create => [maps:get(<<"name">>, R) || {R, undefined} <- Ids],
      existing => [maps:get(<<"name">>, R) || {R, Id} <- Ids, is_integer(Id)],
      errors => Missing ++ Collisions ++ PathErrors,
      pages => length(maps:get(<<"resources">>, Plan)), media => length(maps:get(<<"media">>, Plan))}.

collision(_Row, undefined, _Context) -> [];
collision(#{<<"existing">> := <<"owned">>, <<"name">> := Name}, Id, Context) ->
    case m_rsc:p(Id, ?OWNER, Context) of
        true -> [];
        _ -> [{unowned_name_collision, Name, Id}]
    end;
collision(#{<<"category">> := Category, <<"name">> := Name}, Id, Context) ->
    Family = case Category of <<"image">> -> image; _ -> text end,
    case m_rsc:is_a(Id, Family, Context) of
        true -> [];
        false -> [{category_collision, Name, Id}]
    end.

body_targets(Body) ->
    case re:run(Body, <<"(?:href=[\"']/id/|asset://)([a-zA-Z0-9_]+)">>, [global, {capture, [1], binary}]) of
        nomatch -> [];
        {match, Matches} -> [Name || [Name] <- Matches]
    end.

apply_plan(Dir, Plan, Context) ->
    %% Keywords validate their own complete source before writing. Media comes
    %% first; a failed download never leaves a page with an asset:// placeholder.
    {ok, KeywordReport} = zotonicwww2_subject_import:import(Context),
    MediaResults = [import_media(Dir, R, Context) || R <- maps:get(<<"media">>, Plan)],
    PageResults = [ensure_page(R, Context) || R <- maps:get(<<"resources">>, Plan)],
    IdMap = maps:from_list([{maps:get(<<"name">>, R), resolve(R, Context)} || R <- all_rows(Plan)]),
    BodyResults = [write_page(R, IdMap, Context) || R <- maps:get(<<"resources">>, Plan)],
    EdgeResults = [sync_edges(E, IdMap, Context) || E <- edge_groups(Plan, Context)],
    {ok, #{site => z_context:site(Context), environment => m_site:environment(Context),
        keywords => KeywordReport, media => MediaResults, resources => PageResults,
        texts => BodyResults, connections => EdgeResults}}.

create_props(Row) ->
    Props = #{<<"name">> => maps:get(<<"name">>, Row),
        <<"category_id">> => maps:get(<<"category">>, Row),
        <<"content_group_id">> => default_content_group,
        <<"is_published">> => true, <<"language">> => [en], ?OWNER => true},
    case maps:get(<<"page_path">>, Row, <<>>) of
        Path when is_binary(Path), Path =/= <<>> -> Props#{<<"page_path">> => Path};
        _ -> Props
    end.

ensure_page(Row, Context) ->
    case resolve(Row, Context) of
        undefined ->
            %% Texts are resolved after every destination ID exists.
            Title = maps:get(<<"title">>, maps:get(<<"properties">>, Row)),
            {ok, Id} = m_rsc:insert((create_props(Row))#{<<"title">> => #trans{tr = [{en, Title}]}}, Context),
            remember(Row, Id, Context),
            #{name => maps:get(<<"name">>, Row), id => Id, status => created};
        Id ->
            #{name => maps:get(<<"name">>, Row), id => Id, status => existing}
    end.

remember(#{<<"source_uri">> := <<>>}, _Id, _Context) -> ok;
remember(#{<<"source_uri">> := Uri}, Id, Context) -> m_rsc:remember_uri(Id, Uri, Context).

import_media(Dir, Row, Context) ->
    Existing = resolve(Row, Context),
    Props = case Existing of undefined -> create_props(Row); _ -> #{?OWNER => true} end,
    Title = maps:get(<<"title">>, maps:get(<<"properties">>, Row)),
    Result = case Row of
        #{<<"file">> := File, <<"sha256">> := Hash} ->
            case Existing of
                undefined -> m_media:insert_file(safe_file(Dir, File), Props#{
                    <<"title">> => #trans{tr = [{en, Title}]}, ?MEDIA_HASH => Hash}, Context);
                Id ->
                    case {m_rsc:p(Id, ?MEDIA_HASH, Context), m_media:get(Id, Context)} of
                        {Hash, Medium} when is_map(Medium) -> {ok, Id};
                        _ -> m_media:replace_file(safe_file(Dir, File), Id, Props#{?MEDIA_HASH => Hash}, Context)
                    end
            end;
        #{<<"url">> := <<"https://zotonic.com/", _/binary>> = Url} ->
            case Existing of
                undefined -> m_media:insert_url(Url, Props#{<<"title">> => #trans{tr = [{en, Title}]}}, Context);
                Id ->
                    case m_media:get(Id, Context) of
                        undefined -> m_media:replace_url(Url, Id, #{}, Context);
                        _ -> {ok, Id}
                    end
            end
    end,
    {ok, MediaId} = Result,
    true = is_map(m_media:get(MediaId, Context)),
    remember(Row, MediaId, Context),
    #{name => maps:get(<<"name">>, Row), id => MediaId}.

write_page(Row, IdMap, Context) ->
    Id = maps:get(maps:get(<<"name">>, Row), IdMap),
    Languages = m_rsc:p(Id, language, [], Context),
    TextProps = maps:map(fun(Key, Value) ->
        Text = case Key of <<"body">> -> resolve_body(Value, IdMap, Context); _ -> Value end,
        english(Text, m_rsc:p(Id, Key, Context), Languages)
    end, maps:get(<<"properties">>, Row)),
    Props = TextProps#{?OWNER => true, <<"language">> => lists:usort([en | Languages])},
    #{name => maps:get(<<"name">>, Row), id => Id, status => update_changed(Id, Props, Context)}.

english(Value, #trans{tr = Translations}, _Languages) ->
    #trans{tr = lists:keysort(1, [{en, Value} | proplists:delete(en, Translations)])};
english(Value, Existing, [Language | _]) when Language =/= en, is_binary(Existing), Existing =/= <<>> ->
    #trans{tr = lists:keysort(1, [{en, Value}, {Language, Existing}])};
english(Value, _Existing, _Languages) -> #trans{tr = [{en, Value}]}.

resolve_body(Body0, IdMap, Context) ->
    Body = replace_matches(Body0, <<"<!-- z-media asset://([a-zA-Z0-9_]+)">>,
        fun([_, Name]) ->
            Id = target_id(Name, IdMap, Context),
            <<"<!-- z-media ", (integer_to_binary(Id))/binary>>
        end),
    %% Resolve names to destination IDs (including unnamed adopted production
    %% resources). Source numeric IDs were mapped to names by the plan builder.
    Pattern = <<"(href=[\"']/id/|asset://)([a-zA-Z0-9_]+)">>,
    replace_matches(Body, Pattern, fun([Whole, Prefix, Name]) ->
        Id = target_id(Name, IdMap, Context),
        case Prefix of
            <<"asset://">> ->
                {ok, Url} = z_media_tag:url(Id, [], Context),
                true = Url =/= <<>>,
                z_html:escape(Url);
            _ ->
                binary:replace(Whole, Name, integer_to_binary(Id))
        end
    end).

replace_matches(Body, Pattern, Fun) ->
    case re:run(Body, Pattern, [global, {capture, all, index}]) of
        nomatch -> Body;
        {match, Matches} ->
            {Parts, End} = lists:foldl(fun([{Start, Length} | _] = Groups, {Acc, Pos}) ->
                Values = [binary:part(Body, P, L) || {P, L} <- Groups],
                {[Fun(Values), binary:part(Body, Pos, Start - Pos) | Acc], Start + Length}
            end, {[], 0}, Matches),
            iolist_to_binary(lists:reverse([binary:part(Body, End, byte_size(Body) - End) | Parts]))
    end.

target_id(Name, IdMap, Context) ->
    Id = maps:get(Name, IdMap, m_rsc:rid(Name, Context)),
    true = is_integer(Id) orelse error({unresolved_target, Name}),
    Id.

update_changed(Id, Props0, Context) ->
    Props = z_sanitize:escape_props_check(Props0, Context),
    Changes = maps:filter(fun(Key, Value) -> comparable(m_rsc:p(Id, Key, Context)) =/= comparable(Value) end, Props),
    case map_size(Changes) of
        0 -> unchanged;
        _ -> {ok, Id} = m_rsc:update(Id, Changes, [{is_escape_texts, false}], Context), updated
    end.

%% m_rsc removes empty translations when saving; compare that canonical form so
%% historical pages without summaries do not get rewritten on every import.
comparable(#trans{tr = Translations}) ->
    case [{L, T} || {L, T} <- Translations, T =/= <<>>] of
        [] -> undefined;
        Values -> #trans{tr = lists:keysort(1, Values)}
    end;
comparable(<<>>) -> undefined;
comparable(Value) -> Value.

sync_edges(#{<<"subject">> := Name, <<"predicate">> := Predicate, <<"objects">> := Names}, IdMap, Context) ->
    Id = target_id(Name, IdMap, Context),
    Desired = [target_id(N, IdMap, Context) || N <- Names],
    Current = m_edge:objects(Id, Predicate, Context),
    Ledger = m_rsc:p(Id, ?EDGES, #{}, Context),
    Previous = maps:get(Predicate, Ledger, []),
    %% Only remove edges actually added by this importer on a previous run.
    Owned = [I || I <- Previous, lists:member(I, Desired)] ++ (Desired -- Current),
    Extras = [I || I <- Current, not lists:member(I, Desired), not lists:member(I, Previous)],
    Order = Desired ++ Extras,
    Result = case Order =:= Current of
        true -> unchanged;
        false -> ok = m_edge:set_sequence(Id, Predicate, Order, Context), updated
    end,
    _ = update_changed(Id, #{?EDGES => Ledger#{Predicate => lists:usort(Owned)}}, Context),
    #{subject => Name, predicate => Predicate, status => Result, preserved_extra => length(Extras)}.

%% Include empty groups so removing a predicate from the source also removes its
%% previously imported edges. Resources removed altogether are retained.
edge_groups(Plan, Context) ->
    Groups = maps:get(<<"edges">>, Plan),
    Keys = [{maps:get(<<"subject">>, E), maps:get(<<"predicate">>, E)} || E <- Groups],
    Names = lists:usort([maps:get(<<"name">>, R) || R <- all_rows(Plan)] ++
        [maps:get(<<"subject">>, E) || E <- Groups] ++ [<<"page_start">>]),
    Ids = maps:from_list([{maps:get(<<"name">>, R), resolve(R, Context)} || R <- all_rows(Plan)]),
    Removed = [#{<<"subject">> => Name, <<"predicate">> => Predicate, <<"objects">> => []}
        || Name <- Names,
           Predicate <- maps:keys(m_rsc:p(maps:get(Name, Ids, Name), ?EDGES, #{}, Context)),
           not lists:member({Name, Predicate}, Keys)],
    Groups ++ Removed.

-ifdef(TEST).
empty_translation_test() ->
    ?assertEqual(comparable(undefined), comparable(#trans{tr = [{en, <<>>}]})),
    ?assertNotEqual(comparable(undefined), comparable(#trans{tr = [{nl, <<"Tekst">>}]})).

translation_test() ->
    ?assertEqual(#trans{tr = [{en, <<"New">>}, {nl, <<"Oud">>}]},
        english(<<"New">>, #trans{tr = [{en, <<"Old">>}, {nl, <<"Oud">>}]}, [en, nl])),
    ?assertEqual(#trans{tr = [{en, <<"New">>}, {nl, <<"Oud">>}]}, english(<<"New">>, <<"Oud">>, [nl])).

replacement_test() ->
    ?assertEqual(<<"before [one] between [two] after">>,
        replace_matches(<<"before {one} between {two} after">>, <<"\\{([a-z]+)\\}">>,
            fun([_, Name]) -> <<"[", Name/binary, "]">> end)).

%% Explicit development-only check, compiled with TEST; all rows are rolled back.
integration_test(Context) ->
    development = m_site:environment(Context),
    true = z_acl:is_admin(Context),
    try
        verified = z_db:transaction(fun(Tx) ->
            Suffix = z_string:to_lower(z_ids:id(12)),
            Name = <<"test_documentation_", Suffix/binary>>,
            Row = #{<<"name">> => Name, <<"source_uri">> => <<>>, <<"category">> => <<"text">>,
                <<"properties">> => #{<<"title">> => <<"New title">>, <<"body">> => <<"<p>New body</p>">>}},
            #{id := Id, status := created} = ensure_page(Row, Tx),
            true = m_rsc:p(Id, is_published, Tx),
            #{id := Id, status := existing} = ensure_page(Row, Tx),
            {ok, Id} = m_rsc:update(Id, #{<<"is_published">> => false,
                <<"language">> => [en, nl], <<"body">> => #trans{tr = [{nl, <<"Nederlandse tekst">>}]}}, Tx),
            IdMap = #{Name => Id},
            #{status := updated} = write_page(Row, IdMap, Tx),
            #{status := unchanged} = write_page(Row, IdMap, Tx),
            false = m_rsc:p(Id, is_published, Tx),
            #trans{tr = Trans} = m_rsc:p(Id, body, Tx),
            <<"Nederlandse tekst">> = proplists:get_value(nl, Trans),
            {ok, A} = m_rsc:insert(#{<<"category_id">> => text, <<"title">> => <<"Manual edge">>}, Tx),
            {ok, B} = m_rsc:insert(#{<<"category_id">> => text, <<"title">> => <<"Imported edge">>}, Tx),
            {ok, _} = m_edge:insert(Id, relation, A, Tx),
            Map = IdMap#{<<"a">> => A, <<"b">> => B},
            Edge = #{<<"subject">> => Name, <<"predicate">> => <<"relation">>, <<"objects">> => [<<"b">>, <<"a">>]},
            #{status := updated} = sync_edges(Edge, Map, Tx),
            [B, A] = m_edge:objects(Id, relation, Tx),
            #{status := unchanged} = sync_edges(Edge, Map, Tx),
            Plan = #{<<"resources">> => [Row], <<"media">> => [], <<"edges">> => []},
            [Empty] = [E || #{<<"subject">> := Subject} = E <- edge_groups(Plan, Tx), Subject =:= Name],
            #{status := updated} = sync_edges(Empty, Map, Tx),
            [A] = m_edge:objects(Id, relation, Tx),
            Expected = iolist_to_binary([<<"<a href=\"/id/">>, integer_to_binary(Id), <<"\">one</a>">>]),
            Expected = resolve_body(<<"<a href=\"/id/", Name/binary, "\">one</a>">>, Map, Tx),
            Entry = #{category => text, kind => external, name => Name, title => <<"Wrong title">>,
                body => <<"Wrong body">>, keywords => [], source_path => <<"test">>, source_url => <<"https://example.com">>, props => #{}},
            {ok, #{editorial := 1}} = zotonicwww2_doc_import:sync_external(
                <<"external_2147483643">>, [Entry], <<"editorial-test">>, Tx),
            #trans{tr = Trans} = m_rsc:p(Id, body, Tx),
            false = m_rsc:p(Id, is_published, Tx),
            TrackedName = <<Name/binary, "_tracked">>,
            Source = <<"external_2147483642">>,
            {ok, #{created := 1}} = zotonicwww2_doc_import:sync_external(
                Source, [Entry#{name => TrackedName, category => module}], <<"before-handover">>, Tx),
            TrackedId = m_rsc:rid(TrackedName, Tx),
            {ok, TrackedId} = m_rsc:update(TrackedId, #{?OWNER => true}, Tx),
            true = m_rsc:p(TrackedId, is_published, Tx),
            {ok, #{deprecated := 0}} = zotonicwww2_doc_import:sync_external(Source, [], <<"after-handover">>, Tx),
            true = m_rsc:p(TrackedId, is_published, Tx),
            {rollback, verified}
        end, Context),
        ok
    after z_depcache:flush(Context) end.
-endif.

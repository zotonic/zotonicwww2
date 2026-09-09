%% @doc Import the controlled Zotonic subject vocabulary.
%%
%% The CSV lives in the `doc` directory of the running Zotonic installation.
%% It deliberately does not come from the separately managed documentation
%% checkout: the CSV format and this importer must be deployed together. Its
%% eight facets are represented as direct sub-categories of `keyword`, so the
%% standard admin subject selector groups the imported concepts. Individual
%% subjects remain resources, giving them their own page and connections.
%%
%% Every `keyword_slug` is a canonical identifier. The resource name is derived
%% as `zotonic_topic_<keyword_slug>`. All names are validated for uniqueness and
%% checked against existing resources before the import makes any changes.
%%
%% The import is idempotent. Content hashes avoid needless updates, and removed
%% subjects are unpublished. The `parent_slug` column is synchronized through
%% `subject_topic_broader`.
%% @end

-module(zotonicwww2_subject_import).

-export([
    import/1,
    csv_path/1,
    datamodel_categories/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(CSV_FILENAME, "zotonic_subject_topics.csv").
-define(TOPIC_PREFIX, <<"zotonic_topic_">>).
-define(HEADERS, [
    <<"keyword_slug">>,
    <<"facet">>,
    <<"label">>,
    <<"definition">>,
    <<"parent_slug">>,
    <<"aliases">>,
    <<"programmer_context">>,
    <<"content_editor_context">>,
    <<"zotonic_evidence">>,
    <<"wikidata_id">>,
    <<"wikidata_label">>,
    <<"priority">>,
    <<"source_url">>
]).


-spec import(z:context()) -> {ok, map()} | {error, term()}.
import(Context0) ->
    Context = z_acl:sudo(Context0),
    Filename = csv_path(Context),
    case filelib:is_regular(Filename) of
        false ->
            {error, {enoent, Filename}};
        true ->
            try import_file(Filename, Context) of
                Result -> Result
            catch
                Class:Reason:Stack ->
                    ?LOG_ERROR(#{
                        in => zotonicwww2,
                        text => <<"Subject topic import failed">>,
                        result => error,
                        reason => Reason,
                        class => Class,
                        stack => Stack,
                        file => Filename
                    }),
                    {error, {Class, Reason}}
            end
    end.


-spec csv_path(z:context()) -> file:filename_all().
csv_path(_Context) ->
    filename:join([
        z_path:get_path(),
        "doc",
        ?CSV_FILENAME
    ]).


%% @doc Category fixtures for the site datamodel.
%%
%% These use the same facet definitions as the importer, ensuring that a fresh
%% schema install and a later CSV import agree about names and descriptions.
-spec datamodel_categories() -> [tuple()].
datamodel_categories() ->
    [
        {
            maps:get(name, Facet),
            keyword,
            #{
                <<"title">> => maps:get(title, Facet),
                <<"summary">> => maps:get(summary, Facet),
                <<"body">> => maps:get(body, Facet),
                <<"is_published">> => true,
                <<"is_protected">> => true,
                <<"subject_topic_facet_category">> => true,
                <<"subject_topic_facet">> => maps:get(key, Facet)
            }
        }
        || Facet <- facet_definitions()
    ].


import_file(Filename, Context) ->
    case z_csv_parser:scan_lines(Filename) of
        [?HEADERS | Rows0] ->
            Rows = [ Row || Row <- Rows0, not is_empty_row(Row) ],
            case validate_rows(Rows, Context) of
                ok -> import_valid_rows(Rows, Filename, Context);
                {error, _} = Error -> Error
            end;
        [Header | _] ->
            {error, {invalid_header, Header}};
        [] ->
            {error, empty_csv}
    end.


is_empty_row([]) -> true;
is_empty_row(Row) -> lists:all(fun(Value) -> z_string:trim(Value) =:= <<>> end, Row).


%% @doc Validate every derived resource name before changing the database.
%%
%% Besides protecting against duplicate CSV rows, the normalization check is
%% important because Zotonic normalizes resource names on insert. Without it,
%% two visually different slugs could resolve to the same database name.
validate_rows(Rows, Context) ->
    validate_rows(Rows, 2, #{}, Context).

validate_rows([], _RowNumber, _Names, _Context) ->
    ok;
validate_rows([Row | Rest], RowNumber, Names, Context) ->
    case row_to_map(Row, RowNumber) of
        {ok, Topic} ->
            Slug = maps:get(<<"keyword_slug">>, Topic),
            Name = topic_name(Slug),
            case validate_resource_name(Name) of
                ok ->
                    case remember_resource_name(Name, RowNumber, Names) of
                        {ok, Names1} ->
                            case resource_name_available(Name, Slug, Context) of
                                ok -> validate_rows(Rest, RowNumber + 1, Names1, Context);
                                {error, _} = Error -> Error
                            end;
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error ->
                    {error, {invalid_resource_name, RowNumber, Slug, Error}}
            end;
        {error, _} = Error -> Error
    end.


validate_resource_name(Name) when byte_size(Name) > 80 ->
    {error, too_long};
validate_resource_name(Name) ->
    case z_string:to_name(Name) of
        Name -> ok;
        Normalized -> {error, {normalizes_to, Normalized}}
    end.


remember_resource_name(Name, RowNumber, Names) ->
    case maps:find(Name, Names) of
        {ok, PreviousRow} ->
            {error, {duplicate_resource_name, Name, PreviousRow, RowNumber}};
        error ->
            {ok, Names#{Name => RowNumber}}
    end.


resource_name_available(Name, Slug, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            ok;
        Id ->
            case m_rsc:p_no_acl(Id, <<"subject_topic_slug">>, Context) of
                Slug -> ok;
                ExistingSlug -> {error, {resource_name_in_use, Name, Id, ExistingSlug}}
            end
    end.


import_valid_rows(Rows, Filename, Context) ->
    case ensure_vocabulary(Context) of
        {ok, FacetIds} ->
            case import_rows(Rows, FacetIds, Context) of
                {ok, Report0, Topics} ->
                    case sync_relations(Rows, Topics, Context) of
                        ok ->
                            CurrentNames = [
                                Name
                                || #{name := Name} <- maps:values(Topics)
                            ],
                            case unpublish_removed(CurrentNames, Context) of
                                {ok, Removed} ->
                                    Report = Report0#{
                                        categories => map_size(FacetIds),
                                        subjects => length(Rows),
                                        deprecated => Removed
                                    },
                                    ?LOG_INFO(#{
                                        in => zotonicwww2,
                                        text => <<"Subject topics imported">>,
                                        result => ok,
                                        report => Report,
                                        file => Filename
                                    }),
                                    {ok, Report};
                                {error, _} = Error -> Error
                            end;
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.


ensure_vocabulary(Context) ->
    case m_rsc:rid(keyword, Context) of
        undefined ->
            {error, missing_keyword_category};
        KeywordId ->
            case ensure_keyword_description(KeywordId, Context) of
                ok -> ensure_facet_categories(facet_definitions(), KeywordId, #{}, Context);
                {error, _} = Error -> Error
            end
    end.


ensure_keyword_description(KeywordId, Context) ->
    Props = #{
        <<"summary">> => <<"Subjects used to describe and connect related Zotonic documentation and content.">>,
        <<"body">> => <<"<p>Keywords place documentation, articles and cookbook recipes in a shared subject space. They are grouped by information type, audience, domain, architecture, task, technology and quality.</p>">>
    },
    normalize_update_result(m_rsc:update(KeywordId, Props, Context)).


ensure_facet_categories([], _KeywordId, FacetIds, _Context) ->
    {ok, FacetIds};
ensure_facet_categories([Facet | Rest], KeywordId, FacetIds, Context) ->
    case ensure_facet_category(Facet, KeywordId, Context) of
        {ok, Id} ->
            Key = maps:get(key, Facet),
            ensure_facet_categories(Rest, KeywordId, FacetIds#{Key => Id}, Context);
        {error, _} = Error -> Error
    end.


ensure_facet_category(Facet, KeywordId, Context) ->
    Name = maps:get(name, Facet),
    Props = #{
        <<"title">> => maps:get(title, Facet),
        <<"summary">> => maps:get(summary, Facet),
        <<"body">> => maps:get(body, Facet),
        <<"is_published">> => true,
        <<"is_protected">> => true,
        <<"subject_topic_facet_category">> => true,
        <<"subject_topic_facet">> => maps:get(key, Facet)
    },
    case m_rsc:rid(Name, Context) of
        undefined ->
            Id = m_category:insert(KeywordId, Name, maps:to_list(Props), Context),
            {ok, Id};
        Id ->
            case m_rsc:is_a(Id, category, Context) of
                false ->
                    {error, {facet_name_in_use, Name, Id}};
                true ->
                    case m_rsc:update(Id, Props, Context) of
                        {ok, _} ->
                            case m_category:move_below(Id, KeywordId, Context) of
                                ok -> {ok, Id};
                                {error, _} = Error -> Error
                            end;
                        {error, _} = Error -> Error
                    end
            end
    end.


import_rows(Rows, FacetIds, Context) ->
    import_rows(Rows, 2, FacetIds, #{created => 0, updated => 0, unchanged => 0}, #{}, Context).

import_rows([], _RowNumber, _FacetIds, Report, Topics, _Context) ->
    {ok, Report, Topics};
import_rows([Row | Rest], RowNumber, FacetIds, Report, Topics, Context) ->
    case row_to_map(Row, RowNumber) of
        {ok, Topic} ->
            Facet = maps:get(<<"facet">>, Topic),
            case maps:find(Facet, FacetIds) of
                {ok, CategoryId} ->
                    case upsert_topic(Topic, CategoryId, Context) of
                        {ok, Change, Id, Name} ->
                            Slug = maps:get(<<"keyword_slug">>, Topic),
                            Report1 = maps:update_with(Change, fun(N) -> N + 1 end, 1, Report),
                            Topics1 = Topics#{Slug => #{id => Id, name => Name}},
                            import_rows(Rest, RowNumber + 1, FacetIds, Report1, Topics1, Context);
                        {error, _} = Error -> Error
                    end;
                error ->
                    {error, {unknown_facet, RowNumber, Facet}}
            end;
        {error, _} = Error -> Error
    end.


row_to_map(Row, RowNumber) when length(Row) =:= length(?HEADERS) ->
    Topic = maps:from_list(lists:zip(?HEADERS, Row)),
    Required = [<<"keyword_slug">>, <<"facet">>, <<"label">>, <<"definition">>],
    case lists:all(fun(Key) -> maps:get(Key, Topic) =/= <<>> end, Required) of
        true -> {ok, Topic};
        false -> {error, {missing_required_value, RowNumber}}
    end;
row_to_map(Row, RowNumber) ->
    {error, {invalid_column_count, RowNumber, length(Row)}}.


upsert_topic(Topic, CategoryId, Context) ->
    Slug = maps:get(<<"keyword_slug">>, Topic),
    Name = topic_name(Slug),
    Hash = topic_hash(Topic, CategoryId),
    Props = topic_props(Topic, Name, CategoryId, Hash),
    case m_rsc:rid(Name, Context) of
        undefined ->
            case m_rsc:insert(Props, Context) of
                {ok, Id} -> {ok, created, Id, Name};
                {error, _} = Error -> Error
            end;
        Id ->
            case resource_name_available(Name, Slug, Context) of
                ok -> update_topic(Id, Name, Hash, CategoryId, Props, Context);
                {error, _} = Error -> Error
            end
    end.


update_topic(Id, Name, Hash, CategoryId, Props, Context) ->
    CurrentHash = m_rsc:p_no_acl(Id, <<"subject_topic_hash">>, Context),
    CurrentCategory = m_rsc:p_no_acl(Id, category_id, Context),
    case CurrentHash =:= Hash andalso CurrentCategory =:= CategoryId of
        true ->
            {ok, unchanged, Id, Name};
        false ->
            case m_rsc:update(Id, Props, Context) of
                {ok, _} -> {ok, updated, Id, Name};
                {error, _} = Error -> Error
            end
    end.


topic_props(Topic, Name, CategoryId, Hash) ->
    #{
        <<"name">> => Name,
        <<"category_id">> => CategoryId,
        <<"title">> => maps:get(<<"label">>, Topic),
        <<"summary">> => maps:get(<<"definition">>, Topic),
        <<"body">> => topic_body(Topic),
        <<"is_published">> => true,
        <<"is_protected">> => true,
        <<"subject_topic_hash">> => Hash,
        <<"subject_topic_slug">> => maps:get(<<"keyword_slug">>, Topic),
        <<"subject_topic_facet">> => maps:get(<<"facet">>, Topic),
        <<"subject_topic_parent_slug">> => empty_to_undefined(maps:get(<<"parent_slug">>, Topic)),
        <<"subject_topic_aliases">> => aliases(maps:get(<<"aliases">>, Topic)),
        <<"subject_topic_programmer_context">> => maps:get(<<"programmer_context">>, Topic),
        <<"subject_topic_content_editor_context">> => maps:get(<<"content_editor_context">>, Topic),
        <<"subject_topic_evidence">> => maps:get(<<"zotonic_evidence">>, Topic),
        <<"subject_topic_wikidata_id">> => empty_to_undefined(maps:get(<<"wikidata_id">>, Topic)),
        <<"subject_topic_wikidata_label">> => empty_to_undefined(maps:get(<<"wikidata_label">>, Topic)),
        <<"subject_topic_priority">> => maps:get(<<"priority">>, Topic),
        <<"subject_topic_source_url">> => maps:get(<<"source_url">>, Topic)
    }.


topic_body(Topic) ->
    iolist_to_binary([
        <<"<h2>For programmers</h2><p>">>,
        z_html:escape(maps:get(<<"programmer_context">>, Topic)),
        <<"</p><h2>For content editors</h2><p>">>,
        z_html:escape(maps:get(<<"content_editor_context">>, Topic)),
        <<"</p>">>
    ]).


aliases(<<>>) -> [];
aliases(Value) -> binary:split(Value, <<"|">>, [global, trim_all]).


empty_to_undefined(<<>>) -> undefined;
empty_to_undefined(Value) -> Value.


topic_name(Slug) -> <<(?TOPIC_PREFIX)/binary, Slug/binary>>.


topic_hash(Topic, CategoryId) ->
    z_url:hex_encode_lc(crypto:hash(sha256, term_to_binary({Topic, CategoryId}))).


sync_relations(Rows, Topics, Context) ->
    case m_predicate:name_to_id(subject_topic_broader, Context) of
        {ok, _} -> sync_relations_1(Rows, Topics, Context);
        {error, _} -> {error, missing_subject_topic_broader_predicate}
    end.

sync_relations_1([], _Topics, _Context) ->
    ok;
sync_relations_1([Row | Rest], Topics, Context) ->
    {ok, Topic} = row_to_map(Row, 0),
    Slug = maps:get(<<"keyword_slug">>, Topic),
    ParentSlug = maps:get(<<"parent_slug">>, Topic),
    #{id := Id} = maps:get(Slug, Topics),
    ParentIds = case ParentSlug of
        <<>> -> [];
        _ ->
            case maps:find(ParentSlug, Topics) of
                {ok, #{id := ParentId}} -> [ParentId];
                error -> throw({unknown_parent_slug, Slug, ParentSlug})
            end
    end,
    case m_edge:replace(Id, subject_topic_broader, ParentIds, Context) of
        ok -> sync_relations_1(Rest, Topics, Context);
        {error, _} = Error -> Error
    end.


unpublish_removed(CurrentNames, Context) ->
    case z_db:qmap(
        "select id, name from rsc where name like 'zotonic_topic_%' and is_published",
        Context)
    of
        {ok, Rows} -> unpublish_removed(Rows, CurrentNames, 0, Context);
        {error, _} = Error -> Error
    end.

unpublish_removed([], _CurrentNames, Count, _Context) ->
    {ok, Count};
unpublish_removed([#{<<"id">> := Id, <<"name">> := Name} | Rest], CurrentNames, Count, Context) ->
    case lists:member(Name, CurrentNames) of
        true ->
            unpublish_removed(Rest, CurrentNames, Count, Context);
        false ->
            case m_rsc:update(Id, #{<<"is_published">> => false}, Context) of
                {ok, _} -> unpublish_removed(Rest, CurrentNames, Count + 1, Context);
                {error, _} = Error -> Error
            end
    end.


normalize_update_result({ok, _}) -> ok;
normalize_update_result({error, _} = Error) -> Error.


facet_definitions() ->
    [
        facet(
            <<"information_type">>,
            <<"keyword_information_type">>,
            <<"Information type">>,
            <<"The purpose and reading mode of a document.">>,
            <<"<p>Information-type subjects distinguish learning material, practical procedures, reference material, explanations and other documentation forms.</p>">>),
        facet(
            <<"audience">>,
            <<"keyword_audience">>,
            <<"Audience">>,
            <<"The people for whom the content is primarily useful.">>,
            <<"<p>Audience subjects identify whether content is aimed at editors, site administrators, frontend or backend developers, integrators or operators.</p>">>),
        facet(
            <<"domain">>,
            <<"keyword_domain">>,
            <<"Domain">>,
            <<"The functional area or content-management problem being discussed.">>,
            <<"<p>Domain subjects group content by the capability it explains, such as authoring, media, search, identity, email, integration or operations.</p>">>),
        facet(
            <<"architecture">>,
            <<"keyword_architecture">>,
            <<"Architecture">>,
            <<"The Zotonic component or data-model concept central to the content.">>,
            <<"<p>Architecture subjects identify Zotonic building blocks such as resources, categories, predicates, templates, models, controllers and notifications.</p>">>),
        facet(
            <<"task">>,
            <<"keyword_task">>,
            <<"Task">>,
            <<"The action a reader is trying to perform or understand.">>,
            <<"<p>Task subjects describe intent with verbs such as configure, edit, publish, validate, query, upload, import, schedule and monitor.</p>">>),
        facet(
            <<"data_type">>,
            <<"keyword_data_type">>,
            <<"Data type">>,
            <<"The semantic kind of value accepted, transformed, validated or produced.">>,
            <<"<p>Data-type subjects distinguish text, numbers, booleans, dates and times, collections, structured data, identifiers, URLs, language codes and translated text.</p>">>),
        facet(
            <<"technology">>,
            <<"keyword_technology">>,
            <<"Technology">>,
            <<"A language, protocol, format or platform materially involved in the content.">>,
            <<"<p>Technology subjects cover the standards and systems used by Zotonic, including Erlang/OTP, PostgreSQL, HTTP, MQTT, RDF, JSON-LD and OAuth 2.0.</p>">>),
        facet(
            <<"quality">>,
            <<"keyword_quality">>,
            <<"Quality">>,
            <<"A cross-cutting property that the implementation or workflow should preserve.">>,
            <<"<p>Quality subjects identify concerns such as security, privacy, accessibility, performance, reliability, interoperability and maintainability.</p>">>)
    ].


facet(Key, Name, Title, Summary, Body) ->
    #{
        key => Key,
        name => Name,
        title => Title,
        summary => Summary,
        body => Body
    }.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

topic_name_test() ->
    ?assertEqual(<<"zotonic_topic_search_and_discovery">>, topic_name(<<"search_and_discovery">>)).

resource_name_validation_test() ->
    ?assertEqual(ok, validate_resource_name(<<"zotonic_topic_search_and_discovery">>)),
    ?assertMatch(
        {error, {normalizes_to, _}},
        validate_resource_name(<<"zotonic_topic_Search and discovery">>)
    ),
    ?assertEqual({error, too_long}, validate_resource_name(binary:copy(<<"a">>, 81))).

unique_resource_name_test() ->
    Name = <<"zotonic_topic_search_and_discovery">>,
    ?assertEqual({ok, #{Name => 2}}, remember_resource_name(Name, 2, #{})),
    ?assertEqual(
        {error, {duplicate_resource_name, Name, 2, 3}},
        remember_resource_name(Name, 3, #{Name => 2})
    ).

datamodel_categories_test() ->
    Categories = datamodel_categories(),
    ?assertEqual(8, length(Categories)),
    ?assertEqual(
        [
            <<"keyword_information_type">>,
            <<"keyword_audience">>,
            <<"keyword_domain">>,
            <<"keyword_architecture">>,
            <<"keyword_task">>,
            <<"keyword_data_type">>,
            <<"keyword_technology">>,
            <<"keyword_quality">>
        ],
        [ Name || {Name, keyword, _Props} <- Categories ]
    ).

aliases_test() ->
    ?assertEqual([<<"one">>, <<"two">>], aliases(<<"one|two">>)),
    ?assertEqual([], aliases(<<>>)).

-endif.

%% @doc Public documentation search backed by the Zotonic search facet table.
%%
%% The `important` facet contains titles, summaries and subject keywords. It is
%% searched first using the pg_trgm index maintained by mod_search. If that
%% produces fewer than ?FALLBACK_THRESHOLD matches then the regular Zotonic
%% full-text index is searched as a fallback.
%% @end

%% Copyright 2020-2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_zotonicwww2_search).

-behaviour(zotonic_model).

-export([m_get/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(FALLBACK_THRESHOLD, 8).
-define(DEFAULT_LIMIT, 20).
-define(MAX_LIMIT, 50).
-define(MAX_QUERY_LENGTH, 120).
-define(BROWSE_DEFAULT_LIMIT, 40).
-define(CLUSTER_PREVIEW_LIMIT, 3).
-define(MAX_CLUSTER_DEPTH, 8).
-define(SUBJECT_FACET_ORDER, [
    <<"information_type">>,
    <<"audience">>,
    <<"domain">>,
    <<"architecture">>,
    <<"task">>,
    <<"data_type">>,
    <<"technology">>,
    <<"quality">>
]).


-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"results">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    {ok, {search(Payload, Context), Rest}};
m_get([ <<"results">> | Rest ], _Msg, Context) ->
    {ok, {search(#{}, Context), Rest}};
m_get([ <<"results_page">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    {ok, {search_page(Payload, Context), Rest}};
m_get([ <<"results_page">> | Rest ], _Msg, Context) ->
    {ok, {search_page(#{}, Context), Rest}};
m_get([ <<"category">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    {ok, {browse_category(Payload, Context), Rest}};
m_get([ <<"category">> | Rest ], _Msg, Context) ->
    {ok, {browse_category(#{}, Context), Rest}};
m_get([ <<"category_page">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    {ok, {browse_category_page(Payload, Context), Rest}};
m_get([ <<"category_page">> | Rest ], _Msg, Context) ->
    {ok, {browse_category_page(#{}, Context), Rest}};
m_get([ <<"category_cluster">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    {ok, {category_cluster(Payload, Context), Rest}};
m_get([ <<"category_cluster">> | Rest ], _Msg, Context) ->
    {ok, {category_cluster(#{}, Context), Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


-spec search(map(), z:context()) -> map().
search(Payload, Context) ->
    search(<<"facets">>, Payload, Context).


%% @doc Return a result page without recalculating facets. Used by the endless
%% scroll loader after the initial faceted result page has been rendered.
search_page(Payload, Context) ->
    search(<<"query">>, Payload, Context).


search(SearchName, Payload, Context) ->
    Query = query(maps:get(<<"text">>, Payload, <<>>)),
    Page = positive_integer(maps:get(<<"page">>, Payload, 1), 1, 10000, 1),
    Limit = positive_integer(
        maps:get(<<"limit">>, Payload, ?DEFAULT_LIMIT),
        8,
        ?MAX_LIMIT,
        ?DEFAULT_LIMIT),
    Selected = selected_facets(Payload, Context),
    case z_string:len(Query) >= 2 of
        true -> search(SearchName, Query, Selected, Page, Limit, Context);
        false -> empty_result(Query, Selected)
    end.

search(SearchName, Query, Selected, Page, Limit, Context) ->
    BaseArgs = search_args(Selected, Limit),
    TrigramArgs = BaseArgs#{
        <<"facet:important">> => Query,
        <<"page">> => 1
    },
    case m_search:search(SearchName, TrigramArgs, Context) of
        {ok, Trigram} when Trigram#search_result.total >= ?FALLBACK_THRESHOLD ->
            TrigramPage = case Page of
                1 -> Trigram;
                _ -> search_or_empty(SearchName, TrigramArgs#{ <<"page">> => Page }, Context)
            end,
            result_map(Query, Selected, TrigramPage, TrigramPage, false, Context);
        {ok, Trigram} ->
            FullText = search_or_empty(
                SearchName,
                BaseArgs#{ <<"text">> => Query, <<"page">> => Page },
                Context),
            ResultIds = case Page of
                1 -> merge_ids(Trigram#search_result.result, FullText#search_result.result, Limit);
                _ -> FullText#search_result.result
            end,
            Active = FullText#search_result{ result = ResultIds },
            result_map(Query, Selected, Trigram, Active, true, Context);
        {error, _} ->
            %% During a live migration the new facet columns can briefly be
            %% unavailable. Keep search useful while the facet table is rebuilt.
            FullText = search_or_empty(
                SearchName,
                BaseArgs#{ <<"text">> => Query, <<"page">> => Page },
                Context),
            result_map(Query, Selected, empty_search_result(Limit), FullText, true, Context)
    end.

search_args(Selected, Limit) ->
    Facets = maps:filter(
        fun(_Key, Value) -> Value =/= undefined end,
        #{
            <<"category">> => maps:get(category, Selected),
            <<"subject">> => maps:get(subject, Selected),
            <<"module">> => maps:get(module, Selected)
        }),
    #{
        <<"cat">> => [ <<"text">>, <<"media">> ],
        <<"is_findable">> => true,
        <<"is_published">> => true,
        <<"pagelen">> => Limit,
        <<"facet">> => Facets
    }.

search_or_empty(Name, Args, Context) ->
    case m_search:search(Name, Args, Context) of
        {ok, Result} -> Result;
        {error, _} -> empty_search_result(maps:get(<<"pagelen">>, Args, ?DEFAULT_LIMIT))
    end.

result_map(Query, Selected, Trigram, Active, IsFallback, Context) ->
    #search_result{
        result = Ids,
        total = Total,
        page = Page,
        pages = Pages,
        facets = Facets
    } = Active,
    VisibleFacets = prioritize_selected_facets(
        visible_facets(Facets, Context),
        Selected),
    #{
        query => Query,
        result_ids => Ids,
        result_count => length(Ids),
        total => Total,
        page => Page,
        pages => Pages,
        pager => Active,
        facets => VisibleFacets,
        primary_total => Trigram#search_result.total,
        is_fallback => IsFallback,
        category => maps:get(category, Selected),
        subject => maps:get(subject, Selected),
        module => maps:get(module, Selected)
    }.


%% @doc Keep selected values visible when templates limit a facet to its first
%% entries. The remaining values retain the order returned by mod_search.
prioritize_selected_facets(Facets, Selected) ->
    lists:foldl(
        fun({SelectedKey, FacetName}, Acc) ->
            prioritize_selected_facet(
                maps:get(SelectedKey, Selected, undefined),
                FacetName,
                Acc)
        end,
        Facets,
        [
            {category, <<"category">>},
            {subject, <<"subject">>},
            {module, <<"module">>}
        ]).


prioritize_selected_facet(undefined, _FacetName, Facets) ->
    Facets;
prioritize_selected_facet(SelectedId, FacetName, Facets) ->
    case maps:get(FacetName, Facets, undefined) of
        #{ <<"counts">> := Counts } = Facet ->
            {Selected, Others} = lists:partition(
                fun(#{ <<"value">> := Id }) -> Id =:= SelectedId end,
                Counts),
            Facets#{ FacetName => Facet#{ <<"counts">> => Selected ++ Others } };
        _ ->
            Facets
    end.

empty_result(Query, Selected) ->
    #search_result{ facets = Facets } = Empty = empty_search_result(?DEFAULT_LIMIT),
    #{
        query => Query,
        result_ids => [],
        result_count => 0,
        total => 0,
        page => 1,
        pages => 0,
        pager => Empty,
        facets => Facets,
        primary_total => 0,
        is_fallback => false,
        category => maps:get(category, Selected),
        subject => maps:get(subject, Selected),
        module => maps:get(module, Selected)
    }.

empty_search_result(Limit) ->
    #search_result{
        result = [],
        pagelen = Limit,
        total = 0,
        pages = 0,
        facets = #{}
    }.

merge_ids(Primary, Secondary, Limit) ->
    lists:sublist(Primary ++ [ Id || Id <- Secondary, not lists:member(Id, Primary) ], Limit).


%% @doc Return a paged, faceted view of one documentation category. Facet
%% counts use drill-sideways semantics, keeping alternatives for the selected
%% facet visible. Subject facets are grouped by their keyword subcategory so
%% that additions to the controlled vocabulary appear without template changes.
browse_category(Payload, Context) ->
    browse_category(<<"facets">>, Payload, Context).


%% @doc Return a result page without recalculating facets. Used by the endless
%% scroll loader and when reconstructing an already-scrolled bookmarked URL.
browse_category_page(Payload, Context) ->
    browse_category(<<"query">>, Payload, Context).


browse_category(SearchName, Payload, Context) ->
    Query = query(maps:get(<<"text">>, Payload, <<>>)),
    Page = positive_integer(maps:get(<<"page">>, Payload, 1), 1, 10000, 1),
    Limit = positive_integer(
        maps:get(<<"limit">>, Payload, ?BROWSE_DEFAULT_LIMIT),
        8,
        ?MAX_LIMIT,
        ?BROWSE_DEFAULT_LIMIT),
    CategoryId = selected_facet(category, Payload, Context),
    CategoryFilterId = selected_category_filter(Payload, CategoryId, Context),
    SubjectId = selected_keyword(Payload, Context),
    ModuleId = selected_facet(module, Payload, Context),
    case CategoryId of
        undefined ->
            empty_browse_result(Query, CategoryFilterId, SubjectId, ModuleId, Limit);
        _ ->
            FacetArgs = maps:filter(
                fun(_Key, Value) -> Value =/= undefined end,
                #{
                    <<"category">> => CategoryFilterId,
                    <<"subject">> => SubjectId,
                    <<"module">> => ModuleId
                }),
            Args0 = #{
                <<"cat">> => CategoryId,
                <<"is_findable">> => true,
                <<"is_published">> => true,
                <<"sort">> => <<"pivot_title">>,
                <<"pagelen">> => Limit,
                <<"page">> => Page,
                <<"facet">> => FacetArgs
            },
            Args = case z_string:len(Query) >= 2 of
                true -> Args0#{ <<"text">> => Query };
                false -> Args0
            end,
            case m_search:search(SearchName, Args, Context) of
                {ok, Result} ->
                    browse_result(
                        Query,
                        CategoryId,
                        CategoryFilterId,
                        SubjectId,
                        ModuleId,
                        Result,
                        Context);
                {error, _} ->
                    empty_browse_result(Query, CategoryFilterId, SubjectId, ModuleId, Limit)
            end
    end.


browse_result(Query, CategoryId, CategoryFilterId, SubjectId, ModuleId, Result, Context) ->
    #search_result{
        result = Ids,
        total = Total,
        page = Page,
        pages = Pages,
        facets = Facets0
    } = Result,
    Facets = visible_facets(Facets0, Context),
    #{
        query => Query,
        category => CategoryId,
        selected_category => CategoryFilterId,
        subject => SubjectId,
        module => ModuleId,
        result_ids => Ids,
        total => Total,
        page => Page,
        pages => Pages,
        pager => Result,
        categories => sort_facet_counts(facet_counts(<<"category">>, Facets), Context),
        subject_groups => subject_groups(Facets, Context),
        modules => sort_facet_counts(facet_counts(<<"module">>, Facets), Context)
    }.


%% @doc Build a hierarchical category or keyword view from the controlled
%% subject vocabulary. Each level uses the best remaining keyword facet. If
%% the displayed keywords do not cover every resource then their complement is
%% an "Other" path step. Both named and Other steps can be split again using
%% the best remaining facet. If no useful split remains then the matching
%% resources are returned as a normal, paged list.
category_cluster(Payload, Context) ->
    CategoryId = selected_facet(category, Payload, Context),
    SubjectId = selected_keyword(Payload, Context),
    Path = cluster_path(maps:get(<<"cluster">>, Payload, undefined), Context),
    Page = positive_integer(maps:get(<<"page">>, Payload, 1), 1, 10000, 1),
    Limit = positive_integer(
        maps:get(<<"limit">>, Payload, ?BROWSE_DEFAULT_LIMIT),
        8,
        ?MAX_LIMIT,
        ?BROWSE_DEFAULT_LIMIT),
    case {CategoryId, SubjectId} of
        {undefined, undefined} ->
            empty_cluster_result(Path, Limit);
        _ ->
            case cluster_search(CategoryId, SubjectId, Path, Page, Limit, Context) of
                {ok, Result} ->
                    category_cluster_result(CategoryId, SubjectId, Path, Result, Context);
                {error, _} ->
                    (empty_cluster_result(Path, Limit))#{
                        category => CategoryId,
                        subject => SubjectId
                    }
            end
    end.


category_cluster_result(CategoryId, SubjectId, Path, Result, Context) ->
    #search_result{
        result = Ids,
        total = Total,
        page = Page,
        pages = Pages,
        facets = Facets0
    } = Result,
    Facets = visible_facets(Facets0, Context),
    Groups = subject_groups(Facets, Context),
    SelectedKeywordIds = cluster_keyword_ids(SubjectId, Path),
    ExcludedCategoryIds = lists:usort([
        KeywordCategoryId
        || Id <- SelectedKeywordIds,
           KeywordCategoryId <- [ m_rsc:p(Id, <<"category_id">>, Context) ],
           is_integer(KeywordCategoryId)
    ]),
    Split = zotonicwww2_category_cluster:split(Total, Groups, ExcludedCategoryIds),
    Base = #{
        category => CategoryId,
        subject => SubjectId,
        active => Path =/= [],
        is_other => is_other_cluster(Path),
        path_value => cluster_path_value(Path),
        parent_path_value => cluster_path_value(drop_last(Path)),
        parent_keyword => current_keyword(drop_last(Path)),
        parent_is_other => is_other_cluster(drop_last(Path)),
        current_keyword => current_keyword(Path),
        result_ids => Ids,
        total => Total,
        page => Page,
        pages => Pages,
        pager => Result,
        clusters => []
    },
    case Split of
        #{ category_id := FacetCategoryId, clusters := ClusterCounts } ->
            Base#{
                cluster_facet => FacetCategoryId,
                clusters => cluster_cards(
                    CategoryId,
                    SubjectId,
                    Path,
                    ClusterCounts,
                    Context)
            };
        undefined ->
            Base
    end.


cluster_cards(CategoryId, SubjectId, Path, ClusterCounts, Context) ->
    NamedCards = lists:filtermap(
        fun(#{ <<"value">> := KeywordId }) ->
            NextPath = Path ++ [ {keyword, KeywordId} ],
            case cluster_search(
                CategoryId,
                SubjectId,
                NextPath,
                1,
                ?CLUSTER_PREVIEW_LIMIT,
                Context)
            of
                {ok, #search_result{
                        result = ResultIds,
                        total = Total,
                        facets = Facets0
                    }} ->
                    Facets = visible_facets(Facets0, Context),
                    Groups = subject_groups(Facets, Context),
                    Supporting = zotonicwww2_category_cluster:important_keywords(
                        Total,
                        Groups,
                        cluster_keyword_ids(SubjectId, NextPath),
                        2),
                    {true, #{
                        is_other => false,
                        keyword_id => KeywordId,
                        keywords => [ KeywordId | Supporting ],
                        path_value => cluster_path_value(NextPath),
                        result_ids => ResultIds,
                        total => Total
                    }};
                {error, _} ->
                    false
            end
        end,
        ClusterCounts),
    OtherKeywordIds = [
        KeywordId
        || #{ <<"value">> := KeywordId } <- ClusterCounts
    ],
    case other_cluster_card(CategoryId, SubjectId, Path, OtherKeywordIds, Context) of
        undefined -> NamedCards;
        OtherCard -> NamedCards ++ [ OtherCard ]
    end.


other_cluster_card(CategoryId, SubjectId, Path, OtherKeywordIds, Context) ->
    NextPath = Path ++ [ {other, OtherKeywordIds} ],
    case cluster_search(
        CategoryId,
        SubjectId,
        NextPath,
        1,
        ?CLUSTER_PREVIEW_LIMIT,
        Context)
    of
        {ok, #search_result{ total = 0 }} ->
            undefined;
        {ok, #search_result{
                result = ResultIds,
                total = Total,
                facets = Facets0
            }} ->
            Facets = visible_facets(Facets0, Context),
            Groups = subject_groups(Facets, Context),
            Supporting = zotonicwww2_category_cluster:important_keywords(
                Total,
                Groups,
                cluster_keyword_ids(SubjectId, NextPath),
                2),
            #{
                is_other => true,
                keyword_id => undefined,
                keywords => Supporting,
                path_value => cluster_path_value(NextPath),
                result_ids => ResultIds,
                total => Total
            };
        {error, _} ->
            undefined
    end.


cluster_search(CategoryId, SubjectId, Path, Page, Limit, Context) ->
    BaseProps = maps:filter(
        fun(_Key, Value) -> Value =/= undefined end,
        #{
            <<"cat">> => CategoryId,
            <<"is_findable">> => true,
            <<"is_published">> => true,
            <<"sort">> => <<"pivot_title">>,
            <<"page">> => Page,
            <<"pagelen">> => Limit
        }),
    Base = z_search_props:from_map(BaseProps),
    #{ <<"q">> := Terms } = Base,
    SubjectIds = lists:usort([
        Id
        || Id <- [ SubjectId | [ PathId || {keyword, PathId} <- Path ] ],
           is_integer(Id)
    ]),
    SubjectTerms = [
        #{ <<"term">> => <<"facet:subject">>, <<"value">> => Id }
        || Id <- SubjectIds
    ],
    OtherTerms = other_search_terms(other_keyword_ids(Path)),
    m_search:search(
        <<"subfacets">>,
        Base#{ <<"q">> => Terms ++ SubjectTerms ++ OtherTerms },
        Context).


other_search_terms([]) ->
    [];
other_search_terms(KeywordIds) ->
    [#{
        <<"operator">> => <<"noneof">>,
        <<"terms">> => [
            #{
                <<"term">> => <<"hasanyobject">>,
                <<"value">> => [
                    [ Id, <<"subject">> ]
                    || Id <- KeywordIds
                ]
            }
        ]
    }].


empty_cluster_result(Path, Limit) ->
    Empty = empty_search_result(Limit),
    #{
        category => undefined,
        subject => undefined,
        active => Path =/= [],
        is_other => is_other_cluster(Path),
        path_value => cluster_path_value(Path),
        parent_path_value => cluster_path_value(drop_last(Path)),
        parent_keyword => current_keyword(drop_last(Path)),
        parent_is_other => is_other_cluster(drop_last(Path)),
        current_keyword => current_keyword(Path),
        result_ids => [],
        total => 0,
        page => 1,
        pages => 0,
        pager => Empty,
        clusters => []
    }.


cluster_path(undefined, _Context) ->
    [];
cluster_path(Value, Context) ->
    Tokens = case Value of
        V when is_binary(V) -> binary:split(V, <<",">>, [ global, trim_all ]);
        V when is_integer(V) -> [ V ];
        V when is_list(V) -> V;
        _ -> []
    end,
    Valid = lists:filtermap(
        fun(Token) -> cluster_path_step(Token, Context) end,
        Tokens),
    lists:sublist(unique(Valid), ?MAX_CLUSTER_DEPTH).


cluster_path_step(<<"o-", KeywordIds/binary>>, Context) ->
    Ids = valid_keyword_ids(
        binary:split(KeywordIds, <<"-">>, [ global, trim_all ]),
        Context),
    case Ids of
        [] -> false;
        [_ | _] -> {true, {other, Ids}}
    end;
cluster_path_step(Token, Context) ->
    case valid_keyword_ids([ Token ], Context) of
        [ Id ] -> {true, {keyword, Id}};
        [] -> false
    end.


valid_keyword_ids(Tokens, Context) ->
    unique(lists:filtermap(
        fun(Token) ->
            case m_rsc:rid(Token, Context) of
                Id when is_integer(Id) ->
                    case m_rsc:is_a(Id, keyword, Context)
                        andalso m_rsc:is_visible(Id, Context)
                    of
                        true -> {true, Id};
                        false -> false
                    end;
                _ ->
                    false
            end
        end,
        Tokens)).


unique(List) ->
    unique(List, sets:new([{version, 2}]), []).

unique([], _Seen, Acc) ->
    lists:reverse(Acc);
unique([Id | Rest], Seen, Acc) ->
    case sets:is_element(Id, Seen) of
        true -> unique(Rest, Seen, Acc);
        false -> unique(Rest, sets:add_element(Id, Seen), [ Id | Acc ])
    end.


cluster_path_value([]) ->
    undefined;
cluster_path_value(Path) ->
    iolist_to_binary(lists:join($,, [ cluster_step_value(Step) || Step <- Path ])).


cluster_step_value({keyword, Id}) ->
    integer_to_binary(Id);
cluster_step_value({other, KeywordIds}) ->
    [
        <<"o-">>,
        lists:join($-, [ integer_to_binary(Id) || Id <- KeywordIds ])
    ].


drop_last([]) -> [];
drop_last(Path) -> lists:droplast(Path).


current_keyword([]) -> undefined;
current_keyword(Path) ->
    case lists:last(Path) of
        {keyword, Id} -> Id;
        {other, _} -> undefined
    end.


is_other_cluster([]) -> false;
is_other_cluster(Path) ->
    case lists:last(Path) of
        {other, _} -> true;
        {keyword, _} -> false
    end.


cluster_keyword_ids(Path) ->
    lists:usort(lists:flatten([
        case Step of
            {keyword, Id} -> [ Id ];
            {other, Ids} -> Ids
        end
        || Step <- Path
    ])).


cluster_keyword_ids(undefined, Path) ->
    cluster_keyword_ids(Path);
cluster_keyword_ids(SubjectId, Path) ->
    lists:usort([ SubjectId | cluster_keyword_ids(Path) ]).


other_keyword_ids(Path) ->
    lists:usort(lists:flatten([
        Ids
        || {other, Ids} <- Path
    ])).


empty_browse_result(Query, CategoryFilterId, SubjectId, ModuleId, Limit) ->
    Empty = empty_search_result(Limit),
    #{
        query => Query,
        category => undefined,
        selected_category => CategoryFilterId,
        subject => SubjectId,
        module => ModuleId,
        result_ids => [],
        total => 0,
        page => 1,
        pages => 0,
        pager => Empty,
        categories => [],
        subject_groups => [],
        modules => []
    }.


selected_category_filter(_Payload, undefined, _Context) ->
    undefined;
selected_category_filter(Payload, CategoryId, Context) ->
    case m_rsc:rid(maps:get(<<"category_filter">>, Payload, undefined), Context) of
        Id when is_integer(Id) ->
            case m_category:is_a(Id, CategoryId, Context) of
                true -> Id;
                false -> undefined
            end;
        _ ->
            undefined
    end.


selected_keyword(Payload, Context) ->
    case selected_facet(subject, Payload, Context) of
        Id when is_integer(Id) ->
            case m_rsc:is_a(Id, keyword, Context) of
                true -> Id;
                false -> undefined
            end;
        undefined ->
            undefined
    end.


facet_counts(Name, Facets) ->
    case maps:get(Name, Facets, undefined) of
        #{ <<"counts">> := Counts } -> Counts;
        _ -> []
    end.


subject_groups(Facets, Context) ->
    Groups = lists:foldl(
        fun(#{ <<"value">> := SubjectId } = Count, Acc) ->
            CategoryId = m_rsc:p(SubjectId, <<"category_id">>, Context),
            case m_rsc:p(CategoryId, <<"subject_topic_facet">>, Context) of
                Facet when is_binary(Facet), Facet =/= <<>> ->
                    Group0 = maps:get(Facet, Acc, #{
                        key => Facet,
                        category_id => CategoryId,
                        counts => []
                    }),
                    Counts = maps:get(counts, Group0),
                    Acc#{ Facet => Group0#{ counts => [ Count | Counts ] } };
                _ ->
                    Acc
            end
        end,
        #{},
        facet_counts(<<"subject">>, Facets)),
    [
        Group#{ counts => sort_facet_counts(maps:get(counts, Group), Context) }
        || Facet <- ?SUBJECT_FACET_ORDER,
           {ok, Group} <- [ maps:find(Facet, Groups) ]
    ].


sort_facet_counts(Counts, Context) ->
    lists:sort(
        fun(#{ <<"value">> := A }, #{ <<"value">> := B }) ->
            facet_sort_value(A, Context) =< facet_sort_value(B, Context)
        end,
        Counts).


facet_sort_value(Id, Context) ->
    Title = z_trans:lookup_fallback(m_rsc:p(Id, <<"title">>, Context), Context),
    z_string:to_lower(z_convert:to_binary(Title)).

selected_facets(Payload, Context) ->
    #{
        category => selected_facet(category, Payload, Context),
        subject => selected_facet(subject, Payload, Context),
        module => selected_facet(module, Payload, Context)
    }.

selected_facet(Name, Payload, Context) ->
    Key = atom_to_binary(Name, utf8),
    case m_rsc:rid(maps:get(Key, Payload, undefined), Context) of
        Id when is_integer(Id) ->
            case is_allowed_facet(Name, Id, Context) of
                true -> Id;
                false -> undefined
            end;
        undefined ->
            undefined
    end.

is_allowed_facet(category, Id, Context) ->
    m_rsc:is_a(Id, category, Context);
is_allowed_facet(module, Id, Context) ->
    m_rsc:is_a(Id, module, Context) andalso m_rsc:is_visible(Id, Context);
is_allowed_facet(subject, Id, Context) ->
    m_rsc:is_visible(Id, Context).

visible_facets(undefined, _Context) ->
    #{};
visible_facets(Facets, Context) ->
    maps:map(
        fun
            (<<"category">>, #{ <<"counts">> := Counts } = Facet) ->
                Facet#{ <<"counts">> => [
                    Count
                    || #{ <<"value">> := Id } = Count <- Counts,
                       is_integer(Id)
                ] };
            (Name, #{ <<"counts">> := Counts } = Facet)
                when Name =:= <<"subject">>;
                     Name =:= <<"module">> ->
                Facet#{ <<"counts">> => [
                    Count
                    || #{ <<"value">> := Id } = Count <- Counts,
                       is_integer(Id),
                       m_rsc:is_visible(Id, Context)
                ] };
            (_Name, Facet) ->
                Facet
        end,
        maps:with([ <<"category">>, <<"subject">>, <<"module">> ], Facets)).

query(Value) ->
    Value1 = z_string:sanitize_utf8(z_convert:to_binary(Value)),
    z_string:truncatechars(z_string:trim(Value1), ?MAX_QUERY_LENGTH, <<>>).

positive_integer(Value, Min, Max, Default) ->
    try z_convert:to_integer(Value) of
        N when is_integer(N), N >= Min, N =< Max -> N;
        _ -> Default
    catch
        _:_ -> Default
    end.

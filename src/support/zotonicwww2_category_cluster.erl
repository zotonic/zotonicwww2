%% @doc Select useful, bounded keyword splits for category navigation.
%%
%% Keywords are grouped by their controlled-vocabulary facet. A split always
%% uses one facet, so its choices have a clear shared meaning and do not mix
%% unrelated dimensions. Small, very uneven, or poorly covered splits are
%% rejected; callers can then show the ordinary resource list instead.
%% @end

-module(zotonicwww2_category_cluster).

-export([
    split/3,
    important_keywords/4
]).

-define(MAX_NAMED_CLUSTERS, 9).
-define(MIN_RESOURCES, 8).
-define(MIN_QUALITY, 0.33).


%% @doc Return the best keyword facet split, or `undefined` when none of the
%% available facets divides the result set well enough.
-spec split(non_neg_integer(), [map()], [m_rsc:resource_id()]) -> map() | undefined.
split(Total, Groups, ExcludedCategoryIds) when Total >= ?MIN_RESOURCES ->
    Candidates = lists:filtermap(
        fun(Group) -> candidate(Total, Group, ExcludedCategoryIds) end,
        Groups),
    best_candidate(Candidates);
split(_Total, _Groups, _ExcludedCategoryIds) ->
    undefined.


candidate(Total, #{ category_id := CategoryId, counts := Counts } = Group, ExcludedCategoryIds) ->
    case lists:member(CategoryId, ExcludedCategoryIds) of
        true ->
            false;
        false ->
            Useful0 = [
                Count
                || #{ <<"count">> := N } = Count <- Counts,
                   is_integer(N),
                   N >= 2,
                   N < Total,
                   N * 100 =< Total * 82
            ],
            Useful = lists:sublist(
                lists:sort(fun count_descending/2, Useful0),
                ?MAX_NAMED_CLUSTERS),
            candidate_quality(Total, Group, Useful)
    end.


candidate_quality(_Total, _Group, [_]) ->
    false;
candidate_quality(_Total, _Group, []) ->
    false;
candidate_quality(Total, #{ key := Key } = Group, Useful) ->
    Counts = [ N || #{ <<"count">> := N } <- Useful ],
    Sum = lists:sum(Counts),
    Coverage = min(1.0, Sum / Total),
    Overlap = max(1.0, Sum / Total),
    Entropy = normalized_entropy(Counts, Sum),
    Diversity = min(1.0, length(Counts) / 4),
    Quality = Coverage
        * (0.65 + 0.35 * Entropy)
        * (0.85 + 0.15 * Diversity)
        * facet_weight(Key)
        / Overlap,
    case Quality >= ?MIN_QUALITY of
        true ->
            {true, Group#{
                clusters => Useful,
                quality => Quality
            }};
        false ->
            false
    end.


count_descending(#{ <<"count">> := A, <<"value">> := AId },
                 #{ <<"count">> := B, <<"value">> := BId }) ->
    A > B orelse A =:= B andalso AId =< BId.


normalized_entropy(_Counts, 0) ->
    0.0;
normalized_entropy([_], _Sum) ->
    0.0;
normalized_entropy(Counts, Sum) ->
    Entropy = -lists:sum([
        begin
            P = Count / Sum,
            P * math:log(P)
        end
        || Count <- Counts
    ]),
    Entropy / math:log(length(Counts)).


best_candidate([]) ->
    undefined;
best_candidate([Candidate | Rest]) ->
    lists:foldl(
        fun(#{ quality := Quality } = Next, #{ quality := BestQuality } = Best) ->
            case Quality > BestQuality of
                true -> Next;
                false -> Best
            end
        end,
        Candidate,
        Rest).


%% @doc Pick supporting keywords for a cluster card. Ubiquitous keywords and
%% navigation facets are omitted because they add no distinguishing meaning.
-spec important_keywords(
    non_neg_integer(), [map()], [m_rsc:resource_id()], non_neg_integer()) ->
    [m_rsc:resource_id()].
important_keywords(Total, Groups, ExcludedIds, Limit) when Total > 0, Limit > 0 ->
    Candidates = lists:flatten([
        important_group_keywords(Total, Group, ExcludedIds)
        || Group <- Groups,
           is_descriptive_facet(maps:get(key, Group, undefined))
    ]),
    Sorted = lists:sort(fun important_descending/2, Candidates),
    lists:sublist([ Id || {_Score, Id} <- Sorted ], Limit);
important_keywords(_Total, _Groups, _ExcludedIds, _Limit) ->
    [].


important_group_keywords(Total, #{ key := Key, counts := Counts }, ExcludedIds) ->
    Weight = facet_weight(Key),
    [
        {N / Total * Weight, Id}
        || #{ <<"value">> := Id, <<"count">> := N } <- Counts,
           is_integer(Id),
           is_integer(N),
           N >= 2,
           N < Total,
           not lists:member(Id, ExcludedIds)
    ].


important_descending({AScore, AId}, {BScore, BId}) ->
    AScore > BScore orelse AScore =:= BScore andalso AId =< BId.


is_descriptive_facet(<<"information_type">>) -> false;
is_descriptive_facet(<<"audience">>) -> false;
is_descriptive_facet(_) -> true.


facet_weight(<<"data_type">>) -> 1.10;
facet_weight(<<"domain">>) -> 1.07;
facet_weight(<<"architecture">>) -> 1.04;
facet_weight(<<"task">>) -> 1.00;
facet_weight(<<"technology">>) -> 0.96;
facet_weight(<<"quality">>) -> 0.92;
facet_weight(_) -> 0.80.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_prefers_well_covered_dimension_test() ->
    Groups = [
        group(10, <<"task">>, [{101, 3}, {102, 2}]),
        group(11, <<"data_type">>, [{201, 5}, {202, 4}, {203, 3}])
    ],
    #{ category_id := 11, clusters := Clusters } = split(14, Groups, []),
    ?assertEqual(3, length(Clusters)).

split_rejects_weak_or_small_result_test() ->
    ?assertEqual(undefined, split(7, [group(10, <<"domain">>, [{1, 4}, {2, 3}])], [])),
    ?assertEqual(undefined, split(50, [group(10, <<"domain">>, [{1, 3}, {2, 2}])], [])).

split_excludes_used_dimension_and_caps_result_test() ->
    Counts = [ {Id, 16 - (Id div 2)} || Id <- lists:seq(1, 12) ],
    Groups = [
        group(10, <<"domain">>, [{101, 45}, {102, 35}]),
        group(11, <<"task">>, Counts)
    ],
    #{ category_id := 11, clusters := Clusters } = split(100, Groups, [10]),
    ?assertEqual(?MAX_NAMED_CLUSTERS, length(Clusters)).

important_keywords_omit_generic_and_selected_test() ->
    Groups = [
        group(10, <<"information_type">>, [{1, 10}]),
        group(11, <<"domain">>, [{2, 8}, {3, 5}]),
        group(12, <<"task">>, [{4, 6}])
    ],
    ?assertEqual([4, 3], important_keywords(10, Groups, [2], 3)).

group(CategoryId, Key, Counts) ->
    #{
        category_id => CategoryId,
        key => Key,
        counts => [
            #{ <<"value">> => Id, <<"count">> => Count }
            || {Id, Count} <- Counts
        ]
    }.
-endif.

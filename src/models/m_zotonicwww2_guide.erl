%% @doc Visible guide navigation derived from ordered haspart connections.
%% Shared articles can have several paths; prefer their full section path over
%% legacy shortcuts directly from a guide root. No resource IDs are hardcoded.
-module(m_zotonicwww2_guide).
-behaviour(zotonic_model).
-export([m_get/3, navigation/2]).

-spec m_get(list(), term(), z:context()) -> {ok, {term(), list()}} | {error, term()}.
m_get([<<"navigation">>, Resource | Rest], _Msg, Context) ->
    {ok, {navigation(Resource, Context), Rest}};
m_get(_, _, _) -> {error, unknown_path}.

-spec navigation(m_rsc:resource(), z:context()) -> map() | undefined.
navigation(Resource, Context) ->
    Id = m_rsc:rid(Resource, Context),
    Start = m_rsc:rid(page_start, Context),
    case m_rsc:p(Id, doc_editorial_bundle, Context) =:= true andalso z_acl:rsc_visible(Id, Context) of
        false -> undefined;
        true ->
            Paths = paths(Id, Start, [], Context),
            RequestedRoot = m_rsc:rid(z_context:get_q(<<"guide">>, Context), Context),
            InGuide = [P || [RootId | _] = P <- Paths, RootId =:= RequestedRoot],
            Selected = case InGuide of [] -> Paths; _ -> InGuide end,
            case lists:sort([{-length(P), P} || P <- Selected]) of
                [] -> undefined;
                [{_, Path} | _] ->
                    Root = hd(Path),
                    Parent = case lists:reverse(Path) of [_, P | _] -> P; _ -> undefined end,
                    RootChildren = children(Root, Context),
                    {Sections, Additional} = lists:partition(fun(Child) ->
                        children(Child, Context) =/= []
                    end, RootChildren),
                    Siblings = case Parent =:= Root andalso lists:member(Id, Sections) of
                        true -> Sections;
                        false -> children(Parent, Context)
                    end,
                    {Previous, Next} = neighbours(Id, Siblings, undefined),
                    #{root => Root, path => Path, parent => Parent,
                      siblings => Siblings, previous => Previous, next => Next,
                      children => case Id =:= Root of true -> Sections; false -> children(Id, Context) end,
                      additional => case Id =:= Root of true -> Additional; false -> [] end,
                      alternatives => lists:usort([lists:nth(length(P)-1, P) || P <- Paths,
                          length(P) > 1, lists:nth(length(P)-1, P) =/= Parent])}
            end
    end.

paths(_, _, Seen, _) when length(Seen) >= 8 -> [];
paths(Id, Start, Seen, Context) ->
    case lists:member(Id, Seen) of
        true -> [];
        false ->
            lists:flatmap(fun
                (P) when P =:= Start -> [[Id]];
                (P) -> [Path ++ [Id] || Path <- paths(P, Start, [Id | Seen], Context)]
            end, [P || P <- m_edge:subjects(Id, haspart, Context), z_acl:rsc_visible(P, Context)])
    end.

children(undefined, _) -> [];
children(Id, Context) ->
    [Child || Child <- m_edge:objects(Id, haspart, Context), z_acl:rsc_visible(Child, Context)].

neighbours(_, [], _) -> {undefined, undefined};
neighbours(Id, [Id], Previous) -> {Previous, undefined};
neighbours(Id, [Id, Next | _], Previous) -> {Previous, Next};
neighbours(Id, [Other | Rest], _) -> neighbours(Id, Rest, Other).

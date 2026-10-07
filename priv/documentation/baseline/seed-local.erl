%% Evaluate with file:script/2, binding Context and Mode (audit | seed).
%% Missing-only baseline import for this application's local development site.
%% Existing page bodies are never overwritten. No production destination allowed.
zotonicwww2 = z_context:site(Context),
true = lists:member(m_site:environment(Context), [development, test]),
true = lists:member(Mode, [audit, seed]),
true = z_acl:is_allowed(use, mod_admin, Context),

Dir = filename:join([code:priv_dir(zotonicwww2), "documentation", "baseline"]),
{ok, ManifestBin} = file:read_file(filename:join(Dir, "manifest.json")),
Manifest = z_json:decode(ManifestBin),
<<"https://zotonic.com">> = maps:get(<<"source">>, Manifest),
Rows = maps:get(<<"resources">>, Manifest),

Find = fun(Row) ->
    case m_rsc:rid(maps:get(<<"target_name">>, Row), Context) of
        undefined -> m_rsc:rid(maps:get(<<"source_uri">>, Row), Context);
        Id -> Id
    end
end,
Read = fun(Row) ->
    {ok, Bin} = file:read_file(filename:join(Dir, maps:get(<<"file">>, Row))),
    maps:get(<<"sha256">>, Row) =:= binary:encode_hex(crypto:hash(sha256, Bin), lowercase)
        orelse error({changed_snapshot, maps:get(<<"file">>, Row)}),
    z_json:decode(Bin)
end,

Before = [{Row, Find(Row)} || Row <- Rows],
Results = lists:map(fun
    ({Row, undefined}) when Mode =:= seed ->
        Export = Read(Row),
        %% Import the resource through Zotonic's normal property/media handling.
        %% Restore only reviewed relationships separately after IDs are resolved.
        Options = [is_authoritative, {import_edges, 0},
            {props_forced, #{<<"name">> => maps:get(<<"target_name">>, Row)}}],
        {ok, {Id, _References}} = m_rsc_import:import(Export, Options, Context),
        ok = m_rsc:remember_uri(Id, maps:get(<<"source_uri">>, Row), Context),
        {Row, Id, created};
    ({Row, undefined}) -> {Row, undefined, missing};
    ({Row, Id}) -> {Row, Id, existing}
end, Before),

BySourceId = maps:from_list([
    {maps:get(<<"source_id">>, Row), Id} || {Row, Id, _} <- Results
]),
Resolve = fun(Object) ->
    case maps:find(maps:get(<<"id">>, Object), BySourceId) of
        {ok, Id} -> Id;
        error ->
            case maps:get(<<"name">>, Object, undefined) of
                Name when is_binary(Name), Name =/= <<>> -> m_rsc:rid(Name, Context);
                _ -> m_rsc:rid(maps:get(<<"uri">>, Object), Context)
            end
    end
end,
EdgeResults = lists:flatmap(fun({Row, Id, State}) ->
    Export = Read(Row),
    Edges = maps:get(<<"edges">>, Export, #{}),
    IsImportedCopy = Id =/= undefined andalso
        m_rsc:rid(maps:get(<<"source_uri">>, Row), Context) =:= Id,
    Predicates = case State =:= created orelse IsImportedCopy of
        true -> [<<"haspart">>, <<"depiction">>, <<"subject">>];
        _ -> [<<"haspart">>]
    end,
    lists:filtermap(fun(Predicate) ->
        case maps:find(Predicate, Edges) of
            error -> false;
            {ok, #{<<"objects">> := Objects}} ->
                Sorted = lists:sort(fun(A, B) ->
                    maps:get(<<"seq">>, A) =< maps:get(<<"seq">>, B)
                end, Objects),
                Desired = [Resolve(maps:get(<<"object_id">>, E)) || E <- Sorted],
                Current = case Id of
                    undefined -> [];
                    _ -> m_edge:objects(Id, Predicate, Context)
                end,
                Missing = Desired -- Current,
                Status = case {lists:member(undefined, Desired), Missing, Mode} of
                    {true, _, _} -> unresolved;
                    {false, [], _} ->
                        case [I || I <- Current, lists:member(I, Desired)] =:= Desired of
                            true -> present;
                            false -> local_order_preserved
                        end;
                    {false, _, audit} -> missing;
                    {false, _, seed} ->
                        %% Only fill an incomplete copy whose existing order agrees.
                        %% Never discard extra local edges or undo conversion work.
                        ExpectedExisting = [I || I <- Desired, lists:member(I, Current)],
                        case Current =:= ExpectedExisting of
                            true ->
                                ok = m_edge:set_sequence(Id, Predicate, Desired, Context),
                                Desired = m_edge:objects(Id, Predicate, Context),
                                added;
                            false -> local_changes_preserved
                        end
                end,
                {true, #{name => maps:get(<<"target_name">>, Row),
                    predicate => Predicate, status => Status,
                    missing_count => length(Missing)}}
        end
    end, Predicates)
end, Results),

Report = #{site => z_context:site(Context), environment => m_site:environment(Context),
    mode => Mode, resources => [#{
        source_id => maps:get(<<"source_id">>, Row),
        source_uri => maps:get(<<"source_uri">>, Row),
        source_name => maps:get(<<"source_name">>, Row),
        target_name => maps:get(<<"target_name">>, Row),
        local_id => Id, status => State
    } || {Row, Id, State} <- Results],
    edges => EdgeResults},
Report.

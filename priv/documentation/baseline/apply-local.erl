%% Apply the prepared English revision overlay to already seeded resources.
%% file:script/2 bindings: Context and Mode (audit | apply).
%% No production destination, implicit sudo, resource deletion, or edge replacement.
zotonicwww2 = z_context:site(Context),
true = lists:member(m_site:environment(Context), [development, test]),
true = lists:member(Mode, [audit, apply]),
true = z_acl:is_admin(Context),

Docs = filename:join([code:priv_dir(zotonicwww2), "documentation"]),
{ok, PlanBin} = file:read_file(filename:join([Docs, "prepared", "baseline-import-plan.json"])),
Plan = z_json:decode(PlanBin),
<<"zotonic-baseline-import-plan-v2">> = maps:get(<<"format">>, Plan),
%% Refuse stale preparation before changing anything.
lists:foreach(fun(#{<<"file">> := File, <<"sha256">> := Expected}) ->
    {ok, Bin} = file:read_file(filename:join(Docs, File)),
    Expected = binary:encode_hex(crypto:hash(sha256, Bin), lowercase)
end, maps:get(<<"inputs">>, Plan)),

Find = fun(#{<<"name">> := Name, <<"source_uri">> := Uri}) ->
    case {m_rsc:rid(Name, Context), m_rsc:rid(Uri, Context)} of
        {undefined, Id} -> Id;
        {Id, undefined} -> Id;
        {Id, Id} -> Id;
        Conflict -> error({identity_conflict, Name, Conflict})
    end
end,
Rows = maps:get(<<"resources">>, Plan),
Names = [maps:get(<<"name">>, Row) || Row <- Rows],
true = length(Names) =:= length(lists:usort(Names)),
lists:foreach(fun(Row) ->
    Props = maps:get(<<"properties">>, Row),
    [] = maps:keys(Props) -- [<<"title">>, <<"summary">>, <<"body">>],
    true = lists:all(fun is_binary/1, maps:values(Props)),
    case maps:get(<<"action">>, Row) of
        <<"retain">> -> 0 = map_size(Props);
        <<"replace">> -> 3 = map_size(Props)
    end
end, Rows),
Resolved = [{Row, Find(Row)} || Row <- Rows],
ResolvedIds = [Id || {_, Id} <- Resolved, Id =/= undefined],
true = length(ResolvedIds) =:= length(lists:usort(ResolvedIds)),
Missing = [maps:get(<<"name">>, Row) || {Row, undefined} <- Resolved],
MediaNames = lists:usort(lists:flatmap(fun(Row) ->
    maps:get(<<"required_media">>, Row, [])
end, Rows)),
MediaUrls = maps:from_list([{Name, case m_rsc:rid(Name, Context) of
    undefined -> undefined;
    MediaId ->
        case z_media_tag:url(MediaId, [], Context) of
            {ok, Url} when is_binary(Url), Url =/= <<>> -> Url;
            _ -> undefined
        end
end} || Name <- MediaNames]),
MissingMedia = [Name || {Name, undefined} <- maps:to_list(MediaUrls)],
MissingPredicates = [P || P <- [<<"hasreference">>, <<"subject">>],
    m_rsc:rid(P, Context) =:= undefined],
%% Missing guide/reference or keyword destinations are reported, never guessed.
Deferred = [#{subject => maps:get(<<"name">>, Row), predicate => P, object => N}
    || Row <- Rows, P <- [<<"hasreference">>, <<"subject">>],
       N <- maps:get(P, Row, []), m_rsc:rid(N, Context) =:= undefined],
Ready = Missing =:= [] andalso MissingMedia =:= [] andalso
    MissingPredicates =:= [] andalso Deferred =:= [],

ReplaceMedia = fun(Body) ->
    maps:fold(fun(Name, Url, Acc) ->
        binary:replace(Acc, <<"asset://", Name/binary>>, z_html:escape(Url), [global])
    end, Body, MediaUrls)
end,
%% Preserve all non-English translations. A plain value belongs to the first
%% resource language (English for these snapshots), not to every language.
English = fun(Value, Existing, Languages) ->
    Translations = case Existing of
        {trans, Tr} -> Tr;
        B when is_binary(B), B =/= <<>> ->
            case Languages of
                [Lang | _] when Lang =/= en -> [{Lang, B}];
                _ -> []
            end;
        _ -> []
    end,
    {trans, [{en, Value} | proplists:delete(en, Translations)]}
end,
Results = case {Mode, Ready} of
    {audit, _} -> [];
    {apply, false} -> error({preflight_failed, #{missing_resources => Missing,
        missing_media => MissingMedia, missing_predicates => MissingPredicates,
        deferred_connections => Deferred}});
    {apply, true} ->
        lists:map(fun({Row, Id}) ->
            Props = maps:get(<<"properties">>, Row),
            Languages = m_rsc:p(Id, language, [], Context),
            Update0 = maps:map(fun(Key, Value) ->
                Text = case Key of <<"body">> -> ReplaceMedia(Value); _ -> Value end,
                English(Text, m_rsc:p(Id, Key, Context), Languages)
            end, Props),
            Update = case map_size(Update0) of
                0 -> #{};
                _ -> Update0#{<<"language">> => lists:usort([en | Languages])}
            end,
            Changed = maps:filter(fun(Key, Value) ->
                m_rsc:p(Id, Key, Context) =/= Value
            end, Update),
            case map_size(Changed) of
                0 -> ok;
                _ -> {ok, Id} = m_rsc:update(Id, Changed, Context)
            end,
            lists:foreach(fun(P) ->
                lists:foreach(fun(Name) ->
                    ObjectId = m_rsc:rid(Name, Context),
                    case lists:member(ObjectId, m_edge:objects(Id, P, Context)) of
                        true -> ok;
                        false -> {ok, _} = m_edge:insert(Id, P, ObjectId, Context)
                    end
                end, maps:get(P, Row, []))
            end, [<<"hasreference">>, <<"subject">>]),
            #{name => maps:get(<<"name">>, Row), local_id => Id,
              changed_properties => maps:keys(Changed)}
        end, Resolved)
end,
#{mode => Mode, ready => Ready, missing_resources => Missing,
  missing_media => MissingMedia, missing_predicates => MissingPredicates,
  deferred_connections => Deferred, resources => Results}.

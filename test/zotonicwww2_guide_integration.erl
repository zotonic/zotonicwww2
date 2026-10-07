%% Run against a development site after importing the documentation bundle.
%% Fixture resources and edges are rolled back, including the temporary cycle.
-module(zotonicwww2_guide_integration).
-export([run/1]).

run(Context) ->
    development = m_site:environment(Context),
    true = z_acl:is_admin(Context),
    try
        verified = z_db:transaction(fun(Tx) ->
            Props = #{<<"category_id">> => collection, <<"is_published">> => true,
                <<"doc_editorial_bundle">> => true, <<"title">> => <<"Navigation fixture">>},
            {ok, Root} = m_rsc:insert(Props, Tx),
            {ok, Section} = m_rsc:insert(Props, Tx),
            {ok, First} = m_rsc:insert(Props, Tx),
            {ok, Hidden} = m_rsc:insert(Props#{<<"is_published">> => false}, Tx),
            {ok, Last} = m_rsc:insert(Props, Tx),
            {ok, _} = m_edge:insert(page_start, haspart, Root, Tx),
            ok = m_edge:set_sequence(Root, haspart, [Section, First], Tx),
            ok = m_edge:set_sequence(Section, haspart, [First, Hidden, Last], Tx),
            Public = z_acl:anondo(Tx),
            #{path := [Root, Section, First], previous := undefined, next := Last,
              siblings := [First, Last]} = m_zotonicwww2_guide:navigation(First, Public),
            undefined = m_zotonicwww2_guide:navigation(Hidden, Public),
            #{previous := First, next := undefined} = m_zotonicwww2_guide:navigation(Last, Public),
            {ok, OtherRoot} = m_rsc:insert(Props, Tx),
            {ok, _} = m_edge:insert(page_start, haspart, OtherRoot, Tx),
            {ok, _} = m_edge:insert(OtherRoot, haspart, Section, Tx),
            z_depcache:flush(Tx),
            Selected = z_context:set_q(<<"guide">>, integer_to_binary(OtherRoot), Public),
            #{path := [OtherRoot, Section, First], root := OtherRoot} =
                m_zotonicwww2_guide:navigation(First, Selected),
            %% Cycle protection must not suppress the valid route to the root.
            {ok, _} = m_edge:insert(First, haspart, Section, Tx),
            z_depcache:flush(Tx),
            #{root := Root} = m_zotonicwww2_guide:navigation(Last,
                z_context:set_q(<<"guide">>, integer_to_binary(Root), Public)),
            {rollback, verified}
        end, Context),
        ok
    after z_depcache:flush(Context) end.

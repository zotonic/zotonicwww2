%% @doc Receive authenticated push webhooks for the Zotonic repository.
%% @end

-module(controller_zotonicwww2_webhook).

-export([
    allowed_methods/1,
    content_types_accepted/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_BODY_LENGTH, 1024*1024).
-define(REPOSITORY, <<"zotonic/zotonic">>).
-define(REF, <<"refs/heads/master">>).

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

content_types_accepted(Context) ->
    {[{<<"application">>, <<"json">>, []}], Context}.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    case m_config:get_boolean(site, rebuild_enabled, Context) of
        false ->
            halt_response(404, Context);
        true ->
            case cowmachine_req:req_body(?MAX_BODY_LENGTH, Context) of
                {undefined, Context1} -> halt_response(400, Context1);
                {Body, Context1} -> process_body(Body, Context1)
            end
    end.

process_body(Body, Context) ->
    Signature = cowmachine_req:get_req_header(<<"x-hub-signature-256">>, Context),
    Secret = z_convert:to_binary(m_config:get_value(site, rebuild_secret, Context)),
    case valid_signature(Signature, Secret, Body) of
        false ->
            ?LOG_NOTICE(#{
                in => zotonicwww2,
                text => <<"Rejected GitHub documentation webhook">>,
                result => error,
                reason => invalid_signature,
                peer => m_req:get(peer, Context)
            }),
            halt_response(403, Context);
        true ->
            process_event(Body, Context)
    end.

process_event(Body, Context) ->
    Event = cowmachine_req:get_req_header(<<"x-github-event">>, Context),
    Delivery = cowmachine_req:get_req_header(<<"x-github-delivery">>, Context),
    try z_json:decode(Body) of
        #{
            <<"ref">> := ?REF,
            <<"after">> := Commit,
            <<"repository">> := #{<<"full_name">> := ?REPOSITORY}
        } when Event =:= <<"push">>, is_binary(Delivery), Delivery =/= <<>> ->
            queue_delivery(Delivery, Commit, Context);
        _ ->
            halt_response(202, Context)
    catch
        _:_ -> halt_response(400, Context)
    end.

queue_delivery(Delivery, Commit, Context) ->
    case zotonicwww2_doc_import:register_delivery(Delivery, Commit, Context) of
        duplicate ->
            halt_response(202, Context);
        new ->
            case m_zotonicwww2_git:queue_commit(Commit, Context) of
                {ok, _TaskId} ->
                    {<<"queued">>, Context};
                {error, invalid_commit} ->
                    ok = zotonicwww2_doc_import:unregister_delivery(Delivery, Context),
                    halt_response(400, Context);
                {error, Reason} ->
                    ok = zotonicwww2_doc_import:unregister_delivery(Delivery, Context),
                    ?LOG_ERROR(#{
                        in => zotonicwww2,
                        text => <<"Could not queue GitHub documentation webhook">>,
                        result => error,
                        reason => Reason
                    }),
                    halt_response(503, Context)
            end
    end.

valid_signature(_Signature, <<>>, _Body) ->
    false;
valid_signature(<<"sha256=", Supplied/binary>>, Secret, Body) ->
    Expected = z_url:hex_encode_lc(crypto:mac(hmac, sha256, Secret, Body)),
    secure_equal(Expected, Supplied);
valid_signature(_Signature, _Secret, _Body) ->
    false.

secure_equal(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
    secure_equal(A, B, 0) =:= 0;
secure_equal(_A, _B) -> false.

secure_equal(<<>>, <<>>, Acc) -> Acc;
secure_equal(<<A, ARest/binary>>, <<B, BRest/binary>>, Acc) ->
    secure_equal(ARest, BRest, Acc bor (A bxor B)).

halt_response(Code, Context) ->
    {{halt, Code}, Context}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

signature_test() ->
    Secret = <<"secret">>,
    Body = <<"{\"ref\":\"refs/heads/master\"}">>,
    Digest = z_url:hex_encode_lc(crypto:mac(hmac, sha256, Secret, Body)),
    ?assert(valid_signature(<<"sha256=", Digest/binary>>, Secret, Body)),
    ?assertNot(valid_signature(<<"sha256=00">>, Secret, Body)),
    ?assertNot(valid_signature(undefined, Secret, Body)).

-endif.

%% @doc Manage the Zotonic source checkout and documentation import pipeline.
%%
%% Read and admin action model paths are admin-only. The legacy secret URL is
%% retained for compatibility; new GitHub deliveries use the dedicated webhook
%% controller and HMAC validation.
%% @end

-module(m_zotonicwww2_git).

-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,
    queue/2,
    queue_commit/2,
    task_run/2,
    task_rebuild/1,
    status/1,
    build_docs/1,
    publish_edoc/1,
    ensure_checkout/1,
    clone/1,
    hash/1,
    fetch/1,
    checkout_commit/2,
    git_dir/1,
    edoc_dir/1,
    base_dir/1,
    apps_dir/1,
    fixup_release_notes/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(GIT_URL, "https://github.com/zotonic/zotonic.git").
-define(GIT_BRANCH, "master").
-define(TASK_KEY, <<"documentation-import">>).


-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get(Path, _Payload, Context) ->
    case z_acl:is_admin(Context) of
        true -> m_get_admin(Path, Context);
        false -> {error, eacces}
    end.

m_get_admin([ <<"status">> | Rest ], Context) ->
    {ok, {status(Context), Rest}};
m_get_admin([ <<"hash">> | Rest ], Context) ->
    case hash(Context) of
        {ok, Hash} -> {ok, {Hash, Rest}};
        {error, _} = Error -> Error
    end;
m_get_admin([ <<"rebuild">>, <<"hash">> | Rest ], Context) ->
    {ok, {config(imported_hash, <<>>, Context), Rest}};
m_get_admin(_Path, _Context) ->
    {error, unknown_path}.


-spec m_post(list(binary()), zotonic_model:opt_msg(), z:context()) ->
    {ok, term()} | {error, term()}.
m_post([ <<"rebuild">>, SuppliedSecret ], _Payload, Context) ->
    case m_config:get_boolean(site, rebuild_enabled, Context) of
        true ->
            Secret = m_config:get_value(site, rebuild_secret, Context),
            case secure_equal(z_convert:to_binary(Secret), SuppliedSecret) of
                true ->
                    ?LOG_INFO(#{
                        in => zotonicwww2,
                        text => <<"Documentation update requested and queued">>,
                        result => ok,
                        peer => m_req:get(peer, Context)
                    }),
                    queue(update, Context);
                false ->
                    ?LOG_NOTICE(#{
                        in => zotonicwww2,
                        text => <<"Documentation update request with wrong secret">>,
                        result => error,
                        reason => eacces,
                        peer => m_req:get(peer, Context)
                    }),
                    {error, eacces}
            end;
        false ->
            {error, disabled}
    end;
m_post(_Path, _Payload, _Context) ->
    {error, unknown_path}.


%% @doc Queue an admin action. The shared key ensures queued documentation
%% work is coalesced and never runs in parallel through the pivot task queue.
-spec queue(fetch | rebuild | update | import, z:context()) ->
    {ok, integer()} | {error, term()}.
queue(Action, Context)
    when Action =:= fetch; Action =:= rebuild; Action =:= update; Action =:= import ->
    queue_action(Action, Context).

-spec queue_commit(binary(), z:context()) -> {ok, integer()} | {error, term()}.
queue_commit(Commit, Context) ->
    case is_git_hash(Commit) of
        true -> queue_action({update_commit, Commit}, Context);
        false -> {error, invalid_commit}
    end.

queue_action(Action, Context) ->
    case z_pivot_rsc:insert_task_after(1, ?MODULE, task_run, ?TASK_KEY, [ Action ], Context) of
        {ok, TaskId} = Ok ->
            set_status(<<"queued">>, action_name(Action), #{action => action_name(Action)}, Context),
            ?LOG_INFO(#{
                in => zotonicwww2,
                text => <<"Documentation task queued">>,
                action => action_name(Action),
                task_id => TaskId,
                result => ok
            }),
            Ok;
        {error, _} = Error ->
            Error
    end.


%% @doc Backwards-compatible task entry point used by existing installations.
-spec task_rebuild(z:context()) -> ok.
task_rebuild(Context) ->
    task_run(update, Context).

%% @doc Execute a queued documentation action and persist progress for the admin
%% dashboard. Errors are recorded and consumed; retrying is an explicit action.
-spec task_run(term(), z:context()) -> ok.
task_run(Action, Context) ->
    Started = timestamp(Context),
    set_status(<<"running">>, action_name(Action), #{
        action => action_name(Action),
        started => Started,
        error => <<>>
    }, Context),
    Result = try run_action(Action, Context) of
        R -> R
    catch
        Class:CatchReason:Stack -> {error, {Class, CatchReason, Stack}}
    end,
    case Result of
        {ok, Report} ->
            Finished = timestamp(Context),
            store_report(Report, Context),
            set_status(<<"success">>, <<"complete">>, #{finished => Finished, error => <<>>}, Context),
            ?LOG_INFO(#{
                in => zotonicwww2,
                text => <<"Documentation task completed">>,
                action => action_name(Action),
                report => Report,
                result => ok
            });
        {error, Reason} ->
            ErrorText = format_error(Reason),
            set_status(
                <<"error">>,
                config(import_stage, <<"unknown">>, Context),
                #{finished => timestamp(Context), error => ErrorText},
                Context),
            ?LOG_ERROR(#{
                in => zotonicwww2,
                text => <<"Documentation task failed">>,
                action => action_name(Action),
                result => error,
                reason => Reason
            })
    end,
    ok.

run_action(fetch, Context) ->
    run_steps([
        {<<"checkout">>, fun() -> ensure_checkout(Context) end},
        {<<"fetch">>, fun() -> fetch(Context) end}
    ], #{}, Context);
run_action(rebuild, Context) ->
    run_import_steps([
        {<<"checkout">>, fun() -> ensure_checkout(Context) end},
        {<<"build">>, fun() -> build_docs(Context) end}
    ], true, Context);
run_action(update, Context) ->
    run_import_steps([
        {<<"checkout">>, fun() -> ensure_checkout(Context) end},
        {<<"fetch">>, fun() -> fetch(Context) end},
        {<<"build">>, fun() -> build_docs(Context) end}
    ], true, Context);
run_action({update_commit, Commit}, Context) ->
    run_import_steps([
        {<<"checkout">>, fun() -> ensure_checkout(Context) end},
        {<<"fetch">>, fun() -> fetch_remote(Context) end},
        {<<"select commit">>, fun() -> checkout_commit(Commit, Context) end},
        {<<"build">>, fun() -> build_docs(Context) end}
    ], true, Context);
run_action(import, Context) ->
    run_import_steps([], false, Context);
run_action(Action, _Context) ->
    {error, {unknown_action, Action}}.

run_import_steps(Prefix, IsPublishEdoc, Context) ->
    ImportSteps = [
        {<<"import">>, fun() -> zotonicwww2_beam_doc:import_docs(Context) end},
        {<<"release index">>, fun() -> fixup_release_notes(Context) end},
        {<<"keyword coverage">>, fun() -> zotonicwww2_doc_import:keyword_coverage(Context) end}
    ],
    PublishSteps = case IsPublishEdoc of
        true -> [{<<"publish EDoc">>, fun() -> publish_edoc(Context) end}];
        false -> []
    end,
    Steps = Prefix ++ ImportSteps ++ PublishSteps,
    case run_steps(Steps, #{}, Context) of
        {ok, Report} ->
            case hash(Context) of
                {ok, Commit} ->
                    m_config:set_value(zotonicwww2, imported_hash, Commit, Context),
                    m_config:set_value(site, rebuild_hash, Commit, Context),
                    {ok, Report#{commit => Commit}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

run_steps([], Report, _Context) ->
    {ok, Report};
run_steps([{Stage, Fun} | Rest], Report, Context) ->
    set_status(<<"running">>, Stage, #{}, Context),
    case Fun() of
        ok -> run_steps(Rest, Report, Context);
        {ok, Value} when is_map(Value) -> run_steps(Rest, maps:merge(Report, Value), Context);
        {ok, _Value} -> run_steps(Rest, Report, Context);
        {error, _} = Error -> Error;
        Other -> {error, {unexpected_result, Stage, Other}}
    end.


-spec status(z:context()) -> map().
status(Context) ->
    #{
        status => config(import_status, <<"idle">>, Context),
        stage => config(import_stage, <<"idle">>, Context),
        action => config(import_action, <<>>, Context),
        started => config(import_started, <<>>, Context),
        finished => config(import_finished, <<>>, Context),
        error => config(import_error, <<>>, Context),
        imported_hash => config(imported_hash, m_config:get_value(site, rebuild_hash, Context), Context),
        checkout_hash => value_or_empty(hash(Context)),
        remote_hash => value_or_empty(remote_hash(Context)),
        branch => <<?GIT_BRANCH>>,
        created => config_int(import_created, Context),
        updated => config_int(import_updated, Context),
        unchanged => config_int(import_unchanged, Context),
        deprecated => config_int(import_deprecated, Context),
        total => config_int(import_total, Context),
        keyword_coverage => config_json(import_keyword_coverage, [], Context),
        migration => migration_status(Context)
    }.

migration_status(Context) ->
    try zotonicwww2_doc_import:migration_status(Context)
    catch _:_ -> #{tracked => 0, deprecated => 0, legacy_candidates => 0}
    end.

store_report(Report, Context) ->
    lists:foreach(
        fun({MapKey, ConfigKey}) ->
            case maps:find(MapKey, Report) of
                {ok, Value} -> m_config:set_value(zotonicwww2, ConfigKey, z_convert:to_binary(Value), Context);
                error -> ok
            end
        end,
        [
            {created, import_created},
            {updated, import_updated},
            {unchanged, import_unchanged},
            {deprecated, import_deprecated},
            {total, import_total}
        ]),
    case maps:find(keyword_coverage, Report) of
        {ok, Coverage} ->
            m_config:set_value(
                zotonicwww2,
                import_keyword_coverage,
                z_json:encode(Coverage),
                Context);
        error ->
            ok
    end.

set_status(Status, Stage, Extra, Context) ->
    m_config:set_value(zotonicwww2, import_status, Status, Context),
    m_config:set_value(zotonicwww2, import_stage, Stage, Context),
    set_optional(import_action, maps:get(action, Extra, undefined), Context),
    set_optional(import_started, maps:get(started, Extra, undefined), Context),
    set_optional(import_finished, maps:get(finished, Extra, undefined), Context),
    set_optional(import_error, maps:get(error, Extra, undefined), Context),
    z_mqtt:publish(
        <<"model/zotonicwww2_git/event/status">>,
        #{status => Status, stage => Stage},
        Context),
    ok.

set_optional(_Key, undefined, _Context) -> ok;
set_optional(Key, Value, Context) -> m_config:set_value(zotonicwww2, Key, Value, Context).


%% @doc Ensure the managed checkout is present.
-spec ensure_checkout(z:context()) -> ok | {error, term()}.
ensure_checkout(Context) ->
    case filelib:is_dir(filename:join(git_dir(Context), <<".git">>)) of
        true -> ok;
        false ->
            case clone(Context) of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end
    end.

-spec clone(z:context()) -> {ok, binary()} | {error, term()}.
clone(Context) ->
    Dir = git_dir(Context),
    case filelib:is_dir(Dir) of
        true -> {error, eexist};
        false ->
            Parent = filename:dirname(Dir),
            ok = filelib:ensure_dir(filename:join(Parent, <<".keep">>)),
            run_command(
                "git clone --origin origin --branch " ?GIT_BRANCH " --single-branch " ?GIT_URL " zotonic-git",
                Parent)
    end.

%% @doc Fetch and select the latest commit from the configured branch.
-spec fetch(z:context()) -> {ok, binary()} | {error, term()}.
fetch(Context) ->
    case fetch_remote(Context) of
        {ok, _} -> checkout_ref("origin/" ?GIT_BRANCH, Context);
        {error, _} = Error -> Error
    end.

fetch_remote(Context) ->
    run_gitcmd("git fetch --prune origin " ?GIT_BRANCH, Context).

-spec checkout_commit(binary(), z:context()) -> {ok, binary()} | {error, term()}.
checkout_commit(Commit, Context) ->
    case is_git_hash(Commit) of
        true -> checkout_ref(binary_to_list(Commit), Context);
        false -> {error, invalid_commit}
    end.

checkout_ref(Ref, Context) ->
    case run_gitcmd("git checkout --detach --force " ++ Ref, Context) of
        {ok, _} ->
            case run_gitcmd("git clean -fdx", Context) of
                {ok, _} -> hash(Context);
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.


%% @doc Compile the selected checkout and generate EDoc in its staging path.
%% The checkout is below TMPDIR because some native dependency Makefiles split
%% absolute target paths on whitespace. This is especially relevant on macOS,
%% where the default Zotonic data directory contains "Application Support".
-spec build_docs(z:context()) -> {ok, binary()} | {error, term()}.
build_docs(Context) ->
    BuildDir = filename:join([ git_dir(Context), <<"doc">>, <<"_build">> ]),
    case file:del_dir_r(BuildDir) of
        ok -> run_gitcmds([ "make", "make edocs" ], Context);
        {error, enoent} -> run_gitcmds([ "make", "make edocs" ], Context);
        {error, _} = Error -> Error
    end.

%% @doc Atomically replace the public EDoc tree after a successful import.
-spec publish_edoc(z:context()) -> ok | {error, term()}.
publish_edoc(Context) ->
    Source = filename:join([ git_dir(Context), <<"doc">>, <<"_build">>, <<"edoc">> ]),
    Target = edoc_dir(Context),
    Staged = <<Target/binary, ".next">>,
    Backup = <<Target/binary, ".previous">>,
    case filelib:is_dir(Source) of
        false -> {error, missing_edoc};
        true ->
            ok = filelib:ensure_dir(filename:join(Target, <<".keep">>)),
            ok = delete_dir(Staged),
            case copy_dir(Source, Staged) of
                ok -> publish_staged_edoc(Staged, Target, Backup);
                {error, _} = Error ->
                    _ = delete_dir(Staged),
                    Error
            end
    end.

-spec publish_staged_edoc(Staged, Target, Backup) -> ok | {error, term()}
    when
        Staged :: file:filename_all(),
        Target :: file:filename_all(),
        Backup :: file:filename_all().
publish_staged_edoc(Staged, Target, Backup) ->
    ok = delete_dir(Backup),
    case move_existing(Target, Backup) of
        ok ->
            case file:rename(Staged, Target) of
                ok ->
                    ok = delete_dir(Backup),
                    ok;
                {error, _} = Error ->
                    _ = restore_backup(Backup, Target),
                    _ = delete_dir(Staged),
                    Error
            end;
        {error, _} = Error ->
            _ = delete_dir(Staged),
            Error
    end.

-spec copy_dir(Source, Target) -> ok | {error, term()}
    when
        Source :: file:filename_all(),
        Target :: file:filename_all().
copy_dir(Source, Target) ->
    case file:make_dir(Target) of
        ok ->
            case file:list_dir(Source) of
                {ok, Names} -> copy_dir_entries(Names, Source, Target);
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

copy_dir_entries([], _Source, _Target) ->
    ok;
copy_dir_entries([ Name | Rest ], Source, Target) ->
    SourcePath = filename:join(Source, Name),
    TargetPath = filename:join(Target, Name),
    Result = case filelib:is_dir(SourcePath) of
        true -> copy_dir(SourcePath, TargetPath);
        false -> copy_file(SourcePath, TargetPath)
    end,
    case Result of
        ok -> copy_dir_entries(Rest, Source, Target);
        {error, _} = Error -> Error
    end.

copy_file(Source, Target) ->
    case file:copy(Source, Target) of
        {ok, _BytesCopied} -> ok;
        {error, _} = Error -> Error
    end.

move_existing(Target, Backup) ->
    case filelib:is_dir(Target) of
        true -> file:rename(Target, Backup);
        false -> ok
    end.

restore_backup(Backup, Target) ->
    case filelib:is_dir(Backup) of
        true -> file:rename(Backup, Target);
        false -> ok
    end.

delete_dir(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, _} = Error -> Error
    end.


-spec hash(z:context()) -> {ok, binary()} | {error, term()}.
hash(Context) ->
    run_gitcmd("git rev-parse --verify HEAD", Context).

remote_hash(Context) ->
    run_gitcmd("git rev-parse --verify origin/" ?GIT_BRANCH, Context).

%% @doc Ensure release notes have a stable version ordering.
-spec fixup_release_notes(z:context()) -> ok | {error, term()}.
fixup_release_notes(Context) ->
    #search_result{result = Ids} = z_search:search(
        <<"query">>,
        #{<<"cat">> => releasenotes, <<"id_exclude">> => doc_releasenotes_index},
        1,
        1000,
        Context),
    IdsSorted = filter_zotonicwww2_by_version:zotonicwww2_by_version(Ids, Context),
    case m_edge:update_sequence(doc_releasenotes_index, haspart, IdsSorted, Context) of
        ok -> m_edge:delete(doc_releasenotes_index, haspart, doc_releasenotes_index, Context);
        {error, _} = Error -> Error
    end.

run_gitcmds(Cmds, Context) ->
    lists:foldl(
        fun
            (_Cmd, {error, _} = Error) -> Error;
            (Cmd, {ok, _}) -> run_gitcmd(Cmd, Context)
        end,
        {ok, <<>>},
        Cmds).

run_gitcmd(Cmd, Context) ->
    run_command(Cmd, git_dir(Context)).

run_command(Cmd, Dir) ->
    Options = [sync, stdout, stderr, {cd, unicode:characters_to_list(Dir)}],
    ?LOG_INFO(#{in => zotonicwww2, text => <<"Running documentation command">>, command => Cmd}),
    case exec:run(Cmd, Options) of
        {ok, Output} -> {ok, command_output(Output)};
        {error, _} = Error -> Error
    end.

command_output(Output) ->
    iolist_to_binary([
        Value
        || {Channel, Value} <- Output,
           (Channel =:= stdout orelse Channel =:= stderr)
    ]).

-spec git_dir(z:context()) -> file:filename_all().
%% @doc Return the disposable, space-free checkout path.
%% Native dependencies built with GNU Make can interpret whitespace in an
%% absolute target path as a separator. TMPDIR is normally private per user on
%% macOS; if it contains whitespace then /tmp is used as a safe fallback.
git_dir(Context) ->
    filename:join([
        space_free_tmp_dir(),
        "zotonic-docs",
        atom_to_list(node()),
        atom_to_list(z_context:site(Context)),
        "zotonic-git"
    ]).

-spec space_free_tmp_dir() -> file:filename_all().
space_free_tmp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        [] -> "/tmp";
        Dir ->
            case has_path_whitespace(Dir) of
                true -> "/tmp";
                false -> Dir
            end
    end.

-spec has_path_whitespace(file:filename_all()) -> boolean().
has_path_whitespace(Path) ->
    lists:any(
        fun(Char) -> lists:member(Char, " \t\r\n\v\f") end,
        unicode:characters_to_list(Path)).

-spec edoc_dir(z:context()) -> binary().
edoc_dir(Context) ->
    unicode:characters_to_binary(filename:join([ base_dir(Context), <<"doc">>, <<"edoc">> ])).

-spec apps_dir(z:context()) -> file:filename_all().
apps_dir(Context) -> filename:join([ git_dir(Context), <<"apps">> ]).

-spec base_dir(z:context()) -> file:filename_all().
base_dir(Context) -> z_path:files_subdir_ensure(<<"data">>, Context).

config(Key, Default, Context) ->
    case m_config:get_value(zotonicwww2, Key, Context) of
        undefined -> Default;
        Value -> Value
    end.

config_int(Key, Context) -> z_convert:to_integer(config(Key, 0, Context)).

config_json(Key, Default, Context) ->
    try z_json:decode(z_convert:to_binary(config(Key, <<>>, Context))) of
        Value when is_list(Value) -> Value;
        _ -> Default
    catch
        _:_ -> Default
    end.

timestamp(Context) ->
    iolist_to_binary(z_datetime:format_utc(calendar:universal_time(), "c", Context)).

value_or_empty({ok, Value}) -> z_string:trim(Value);
value_or_empty({error, _}) -> <<>>.

format_error(Reason) ->
    z_string:truncate(iolist_to_binary(io_lib:format("~p", [Reason])), 2000).

action_name({update_commit, _}) -> <<"update">>;
action_name(Action) when is_atom(Action) -> atom_to_binary(Action).

is_git_hash(Hash) when is_binary(Hash), byte_size(Hash) =:= 40 -> is_hex(Hash);
is_git_hash(Hash) when is_binary(Hash), byte_size(Hash) =:= 64 -> is_hex(Hash);
is_git_hash(_) -> false.

is_hex(<<>>) -> true;
is_hex(<<C, Rest/binary>>)
    when C >= $0, C =< $9; C >= $a, C =< $f; C >= $A, C =< $F ->
    is_hex(Rest);
is_hex(_) -> false.

secure_equal(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
    secure_equal(A, B, 0) =:= 0;
secure_equal(_A, _B) -> false.

secure_equal(<<>>, <<>>, Acc) -> Acc;
secure_equal(<<A, ARest/binary>>, <<B, BRest/binary>>, Acc) ->
    secure_equal(ARest, BRest, Acc bor (A bxor B)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

git_hash_test() ->
    ?assert(is_git_hash(<<"0123456789abcdef0123456789abcdef01234567">>)),
    ?assertNot(is_git_hash(<<"0123456789abcdef">>)),
    ?assertNot(is_git_hash(<<"z123456789abcdef0123456789abcdef01234567">>)).

secure_equal_test() ->
    ?assert(secure_equal(<<"same">>, <<"same">>)),
    ?assertNot(secure_equal(<<"same">>, <<"diff">>)),
    ?assertNot(secure_equal(<<"short">>, <<"longer">>)).

path_whitespace_test() ->
    ?assert(has_path_whitespace("/Library/Application Support/zotonic")),
    ?assert(has_path_whitespace("/tmp/a\tb")),
    ?assertNot(has_path_whitespace("/private/var/folders/zotonic")),
    ?assertNot(has_path_whitespace(<<"/tmp/zotonic">>)).

-endif.

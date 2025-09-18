%% @doc Model for fetching and updating the Git clone of Zotonic.
%% The Git checkout is located in "priv/data/zotonic-git".
%%
%% This file is a model. You will see functions like 'm_get' that
%% implement the model behaviour. Models are always located in the
%% directory 'models' and their filename always start with 'm_'.
%%
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2025 Marc Worrell
%% @end

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

%% Note that the filename starts with m_, followed by the module name.
%% This ensures that there are no unexpected name clashes, which could
%% have easily occured if the module was named something like 'm_git'.

-module(m_zotonicwww2_git).

%% Model behaviour, from zotonic_core/src/behaviours/
%% Only the m_get/3 function is required.
-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,

    task_rebuild/1,

    build_edoc/1,
    clone/1,
    hash/1,
    pull/1,
    git_dir/1,
    edoc_dir/1,
    doc_dir/1,
    base_dir/1,
    apps_dir/1,
    fixup_release_notes/1
    ]).

%% Include the central definitions of Zotonic. Useful for macro
%% definitions like ?DEBUG.
-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Handle GET requests for this model. Can be called from the
%% templates (m.zotonicwww2_git), the API (/api/model/zotonicwww2_git/get/...)
%% or via MQTT (topic model/zotonicwww2_git/get).
%%
%% The first argument is the split path of the request (after 'get').
%%
%% The second argument is the MQTT messages, if any. For template calls
%% this could be 'undefined'. API calls also construct a MQTT message, as
%% the API routes calls via the MQTT tree and the zotonic_model.erl (in
%% zotonic_core/src/support).
%%
%% The m_get function consumes as much of the Path as is needed, it must
%% return its result together with the unconsumed part of the path. The
%% zotonic_model functions will do a further lookup of the Path remainder
%% in the return value.
%%
%% On an error an error tuple should be returned. For the template routines
%% this maps to 'undefined' and is ignored. The API and MQTT will return
%% a payload with the error to the caller.
-spec m_get( Path :: list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get( [ <<"hash">> | Rest ], _Payload, Context) ->
    % Only for authenticated users, as we are running a command line
    % program and don't want to do that to anonymous bots and users.
    case z_auth:is_auth(Context) of
        true ->
            % Return the hash of the current checkout
            case hash(Context) of
                {ok, Hash} ->
                    % Note the 'Rest', this is the non-consumed part of the
                    % path. It is often used by templates for further lookups
                    % in returned maps or other structured values.
                    {ok, {Hash, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;
m_get( [ <<"rebuild">>, <<"hash">> | Rest ], _Payload, Context) ->
    % The task_rebuild stores the hash of the last rebuild in the
    % config key site.rebuild_hash.
    Hash = m_config:get_value(site, rebuild_hash, Context),
    {ok, {Hash, Rest}}.


%% This is a post handler for HTTP posts to `/api/model/zototonicwww2_git/post`
%% and MQTT publish to "model/zotonicwww2_git/post"
%% Note that a post handler always consumes the whole path.
-spec m_post( Path :: list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_post( [ <<"rebuild">>, Secret ], _Payload, Context) ->
    % Compare the value in the config tables with the passed secret.
    % Config values can be set with m_config:set_value/4 or in the
    % admin on "/admin/config"
    case m_config:get_value(site, rebuild_secret, Context) of
        Secret ->
            % As a build takes a long time we schedule a build task.
            % The task is slightly delayed so that repetitive pushes
            % are started after a small period of inactivity.
            z_pivot_rsc:insert_task_after(
                    10,             % Seconds or a date
                    ?MODULE,        % This module
                    task_rebuild,   % Prepend task functions with 'task_'
                    <<>>,           % Give a key for multiple tasks with same mod:fun
                    [],             % Arguments, non needed for this task
                    Context),
            {ok, <<"queued">>};
        _ ->
            % Log a message
            ?LOG_NOTICE("Docs rebuild request with wrong secret from ~p", [ m_req:get(peer, Context) ]),
            % Try to use posix error codes
            {error, eacces}
    end.


%% @doc Documentation rebuild task scheduled by the 'm_post' handler for
%% 'docs-rebuild' above. This task is executed by z_pivot_rsc. Only a single
%% task is executed in parallel.
-spec task_rebuild( z:context() ) -> ok | {error, term()}.
task_rebuild(Context) ->
    z_utils:pipeline([
            fun pull/1,
            fun build_edoc/1,
            fun zotonicwww2_beam_doc:import_docs/1,
            fun fixup_release_notes/1,
            fun() ->
                {ok, Hash} = hash(Context),
                ?LOG_INFO("Rebuild of docs success for '~s'", [ Hash ]),
                m_config:set_value(site, rebuild_hash, Hash, Context),
                ok
            end
        ],
        [ Context ]).


%% @doc Clone a fresh checkout of the zotonic repository. Only running if
%% the priv/data/zotonic-git directory is not present.
-spec clone( z:context() ) -> {ok, binary()} | {error, term()}.
clone(Context) ->
    Dir = git_dir(Context),
    case filelib:is_file(Dir) of
        true ->
            {error, eexist};
        false ->
            DataDir = unicode:characters_to_list( filename:dirname(Dir) ),
            ok = z_filelib:ensure_dir(Dir),
            Cmd = "git clone https://github.com/zotonic/zotonic.git zotonic-git",
            Options = [
                sync,
                stdout,
                {cd, DataDir}
            ],
            ?LOG_INFO("Command: \"~s\"", [ Cmd ]),
            case exec:run(Cmd, Options) of
                {ok, [ {stdout, Output} ]} ->
                    Output1 = filter_output(Output),
                    ?LOG_INFO("Command output: ~s", [ Output1 ]),
                    {ok, iolist_to_binary(Output)};
                {ok, []} ->
                    ?LOG_INFO("Command output: "),
                    {ok, <<>>};
                {error, _} = Error ->
                    ?LOG_ERROR("Command \"~s\" error: ~p", [ Cmd, Error ]),
                    Error
            end
    end.

%% Remove escape sequences, especially from the rebar3 output which
%% adds color escapes.
filter_output(Output) ->
    O1 = iolist_to_binary(Output),
    filter_output_1(O1, <<>>).

filter_output_1(<<>>, Acc) ->
    Acc;
filter_output_1(<<C, R/binary>>, Acc) when C =:= 9; C =:= 10; C =:= 13 ->
    filter_output_1(R, <<Acc/binary, C>>);
filter_output_1(<<C, R/binary>>, Acc) when C < 32 ->
    filter_output_1(R, Acc);
filter_output_1(<<C, R/binary>>, Acc) ->
    filter_output_1(R, <<Acc/binary, C>>).


%% @doc Build Zotonic, the edoc and move them to the edoc_dir. Returns
%% an error or the current hash. This command takes a long time to run.
-spec build_edoc( z:context() ) -> {ok, binary()} | {error, term()}.
build_edoc(Context) ->
    ok = z_filelib:ensure_dir(edoc_dir(Context)),
    run_gitcmds([
            "rm -rf doc/_build",
            "make",
            "make edocs",
            "rm -rf ../doc/edoc",
            "mv doc/_build/edoc ../doc/."
        ],
        Context).


%% @doc Return the current checkout hash of the git repo.
-spec hash( z:context() ) -> {ok, binary()} | {error, term()}.
hash(Context) ->
    run_gitcmd("git log -1 --pretty=format:%H", Context).


%% @doc Ensure that all release notes are a part of the release notes collection.
fixup_release_notes(Context) ->
    #search_result{ result = Ids } = z_search:search(
        <<"query">>,
        #{
            <<"cat">> => releasenotes,
            <<"id_exclude">> => doc_releasenotes_index
        },
        1,
        1000,
        Context),
    IdsSorted = filter_zotonicwww2_by_version:zotonicwww2_by_version(Ids, Context),
    m_edge:update_sequence(doc_releasenotes_index, haspart, IdsSorted, Context),
    m_edge:delete(doc_releasenotes_index, haspart, doc_releasenotes_index, Context),
    ok.

%% @doc Pull changes in the 'priv/data/zotonic-git' directory. Returns the
%% new current hash.
-spec pull( z:context() ) -> {ok, binary()} | {error, term()}.
pull(Context) ->
    ?LOG_INFO(#{
        in => zotonicwww2,
        text => <<"Pulling new version">>,
        directory => git_dir(Context)
    }),
    _ = run_gitcmd("git checkout -- .", Context),
    case run_gitcmd("git clean -f", Context) of
        {ok, _} ->
            case run_gitcmd("git pull", Context) of
                {ok, _} ->
                    hash(Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Run a series of commands in the 'priv/data/zotonic-git' directory.
-spec run_gitcmds( list( string() ), z:context() ) -> {ok, binary()} | {error, term()}.
run_gitcmds(Cmds, Context) ->
    lists:foldl(
        fun
            (_Cmd, {error, _} = Error) ->
                Error;
            (Cmd, {ok, _}) ->
                run_gitcmd(Cmd, Context)
        end,
        {ok, <<>>},
        Cmds).

%% @doc Run a supervised command in the 'priv/data/zotonic-git' directory.
-spec run_gitcmd( string(), z:context() ) -> {ok, binary()} | {error, term()}.
run_gitcmd(Cmd, Context) ->
    Dir = git_dir(Context),
    Options = [
        sync,
        stdout,
        {cd, unicode:characters_to_list(Dir)}
    ],
    ?LOG_INFO("Command: \"~s\"", [ Cmd ]),
    case exec:run(Cmd, Options) of
        {ok, [ {stdout, Output} ]} ->
            ?LOG_INFO("Command output: ~s", [ Output ]),
            {ok, iolist_to_binary(Output)};
        {ok, []} ->
            ?LOG_INFO("Command output: "),
            {ok, <<>>};
        {error, _} = Error ->
            ?LOG_ERROR("Command \"~s\" error: ~p", [ Cmd, Error ]),
            Error
    end.

%% @doc Return the directory of the git checkout.
-spec git_dir( z:context() ) -> file:filename_all().
git_dir(Context) ->
    filename:join([ base_dir(Context), <<"zotonic-git">> ]).

%% @doc Return the directory for the generated edoc documentation (from the erlang sources)
-spec edoc_dir( z:context() ) -> file:filename_all().
edoc_dir(Context) ->
    filename:join([ base_dir(Context), <<"doc">>, <<"edoc">> ]).

%% @doc Return the directory for the generated html documentation (from ReStructuredText)
-spec doc_dir( z:context() ) -> file:filename_all().
doc_dir(Context) ->
    filename:join([ base_dir(Context), <<"doc">>, <<"html">> ]).

%% @doc Return the directory for the zotonic apps sources
-spec apps_dir( z:context() ) -> file:filename_all().
apps_dir(Context) ->
    filename:join([ git_dir(Context), <<"apps">> ]).

%% @doc Return the base directory for all data.
-spec base_dir( z:context() ) -> file:filename_all().
base_dir(Context) ->
    z_path:files_subdir_ensure(<<"data">>, Context).


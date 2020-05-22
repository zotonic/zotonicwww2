%% @doc Model for fetching and updating the Git clone of Zotonic.
%% The Git checkout is located in "priv/data/zotonic-git".
%%
%% This file is a model. You will see functions like 'm_get' that
%% implement the model behaviour. Models are always located in the
%% directory 'models' and their filename always start with 'm_'.
%%
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell

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

    build_edoc/1,
    build_doc/1,
    clone/1,
    hash/1,
    pull/1,
    git_dir/1,
    edoc_dir/1,
    doc_dir/1
    ]).

%% @doc Handle GET requests for this model. Can be called from the
%% templates (m.zotonicwww2_git), the API (/api/model/zotonic_www2/get/...)
%% or via MQTT (topic model/zotonic_www2/get).
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
    {ok, {hash(Context), Rest}}.



%% @doc Build Zotonic, the html docs and move them to the doc_dir. Returns
%% an error or the current hash. This command takes a long time to run.
-spec build_doc( z:context() ) -> {ok, binary()} | {error, term()}.
build_doc(Context) ->
    ok = z_filelib:ensure_dir(doc_dir(Context)),
    run_gitcmds([
            "rm -rf doc/_build",
            "make docs",
            "rm -rf ../doc/html",
            "mv doc/_build/html ../doc/."
        ],
        Context).


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
            lager:info("Command: \"~s\"", [ Cmd ]),
            case exec:run(Cmd, Options) of
                {ok, [ {stdout, Output} ]} ->
                    lager:info("Command output: ~s", [ Output ]),
                    {ok, iolist_to_binary(Output)};
                {ok, []} ->
                    lager:info("Command output: "),
                    {ok, <<>>};
                {error, _} = Error ->
                    lager:error("Command \"~s\" error: ~p", [ Cmd, Error ]),
                    Error
            end
    end.

%% @doc Build Zotonic, the edoc and move them to the edoc_dir. Returns
%% an error or the current hash. This command takes a long time to run.
-spec build_edoc( z:context() ) -> {ok, binary()} | {error, term()}.
build_edoc(Context) ->
    ok = z_filelib:ensure_dir(edoc_dir(Context)),
    run_gitcmds([
            "rm -rf doc/_build",
            % "make",
            "make edocs",
            "rm -rf ../doc/edoc",
            "mv doc/_build/edoc ../doc/."
        ],
        Context).


%% @doc Return the current checkout hash of the git repo.
-spec hash( z:context() ) -> {ok, binary()} | {error, term()}.
hash(Context) ->
    run_gitcmd("git log -1 --pretty=format:%H", Context).


%% @doc Pull changes in the 'priv/data/zotonic-git' directory. Returns the
%% new current hash.
-spec pull( z:context() ) -> {ok, binary()} | {error, term()}.
pull(Context) ->
    case run_gitcmd("git pull", Context) of
        {ok, _} ->
            hash(Context);
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
    lager:info("Command: \"~s\"", [ Cmd ]),
    case exec:run(Cmd, Options) of
        {ok, [ {stdout, Output} ]} ->
            lager:info("Command output: ~s", [ Output ]),
            {ok, iolist_to_binary(Output)};
        {ok, []} ->
            lager:info("Command output: "),
            {ok, <<>>};
        {error, _} = Error ->
            lager:error("Command \"~s\" error: ~p", [ Cmd, Error ]),
            Error
    end.

%% @doc Return the directory of the git checkout.
-spec git_dir( z:context() ) -> file:filename_all().
git_dir(Context) ->
    filename:join([ z_path:site_dir(Context), <<"priv">>, <<"data">>, <<"zotonic-git">> ]).

%% @doc Return the directory for the generated edoc documentation (from the erlang sources)
-spec edoc_dir( z:context() ) -> file:filename_all().
edoc_dir(Context) ->
    filename:join([ z_path:site_dir(Context), <<"priv">>, <<"data">>, <<"doc">>, <<"edoc">> ]).

%% @doc Return the directory for the generated html documentation (from ReStructuredText)
-spec doc_dir( z:context() ) -> file:filename_all().
doc_dir(Context) ->
    filename:join([ z_path:site_dir(Context), <<"priv">>, <<"data">>, <<"doc">>, <<"html">> ]).


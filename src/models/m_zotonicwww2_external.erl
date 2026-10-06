%% @doc Admin-only external documentation registry. Model reads and signed
%% postbacks require administrator access. Workers use the private table API.

%% Copyright 2026 Marc Worrell
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

-module(m_zotonicwww2_external).
-moduledoc("
Admin-only external documentation registry. Model reads and signed
postbacks require administrator access. Workers use the private table API.

The `list` model path returns repository settings, processing state, and
import reports. Administrator postbacks add and edit repositories, queue
imports, and deprecate their documentation. Deprecation unpublishes pages,
moves them into the deprecated documentation group, and pauses updates.
The table access functions are internal APIs, not public model paths.
").
-behaviour(zotonic_model).
-export([m_get/3, event/2, install/1, queue_all/1, get/2, queue/2, deprecate/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(zotonicwww2_external, Context) of
        true -> ok;
        false ->
            [] = z_db:q("create table zotonicwww2_external (
                id serial primary key,
                title text not null,
                git_url text not null,
                website_url text not null default '',
                branch text not null default '',
                hex_package text not null default '',
                creator_id integer references rsc(id) on delete set null,
                created timestamptz not null default now(),
                last_fetched timestamptz,
                last_imported timestamptz,
                git_commit text,
                status text not null default 'new',
                error text,
                report text not null default '[]',
                is_enabled boolean not null default true
            )", Context),
            z_db:flush(Context)
    end.

-spec m_get(list(), term(), z:context()) -> {ok, {term(), list()}} | {error, term()}.
m_get([<<"list">> | Rest], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, Rows} = z_db:qmap("select * from zotonicwww2_external order by title, id", Context),
            {ok, {[with_report(R) || R <- Rows], Rest}};
        false -> {error, eacces}
    end;
m_get(_, _, _) -> {error, unknown_path}.

with_report(Row) ->
    Report = z_json:decode(maps:get(<<"report">>, Row)),
    Imported = length([R || #{<<"rsc_id">> := Id} = R <- Report, is_integer(Id)]),
    Row#{<<"report">> => Report, <<"imported_count">> => Imported,
        <<"skipped_count">> => length(Report) - Imported}.

%% Internal use only; intentionally not a public model path.
-spec get(integer(), z:context()) -> {ok, map()} | {error, term()}.
get(Id, Context) ->
    z_db:qmap_row("select * from zotonicwww2_external where id = $1", [Id], Context).

-spec event(term(), z:context()) -> z:context().
event(Event, Context) ->
    case z_acl:is_admin(Context) of
        true -> admin_event(Event, Context);
        false -> z_render:growl_error(?__("Administrator access required.", Context), Context)
    end.

admin_event(#postback{message = {new, _Args}}, Context) ->
    z_render:dialog(?__("Add repository", Context), "_admin_external_module_form.tpl", [], Context);
admin_event(#postback{message = {edit, Args}}, Context) ->
    repository_dialog(proplists:get_value(id, Args), ?__("Edit repository", Context),
        "_admin_external_module_form.tpl", Context);
admin_event(#postback{message = {report, Args}}, Context) ->
    repository_dialog(proplists:get_value(id, Args), ?__("Import report", Context),
        "_admin_external_module_report.tpl", Context);
admin_event(#submit{message = {save, Args}}, Context) ->
    Id = proplists:get_value(id, Args),
    case save(Id, Context) of
        {ok, SavedId} ->
            feedback(queue(SavedId, Context), Context);
        {error, _} = Error -> feedback(Error, Context)
    end;
admin_event(#postback{message = {deprecate, Args}}, Context) ->
    feedback(deprecate(proplists:get_value(id, Args), Context), Context);
admin_event(#postback{message = {fetch, Args}}, Context) ->
    feedback(queue(proplists:get_value(id, Args), Context), Context);
admin_event(_, Context) -> Context.

%% Load current registry data only after the event's administrator check.
repository_dialog(Id, Title, Template, Context) when is_integer(Id), Id > 0 ->
    case get(Id, Context) of
        {ok, Repo} -> z_render:dialog(Title, Template, [{repo, with_report(Repo)}], Context);
        {error, _} = Error -> feedback(Error, Context)
    end;
repository_dialog(_, _, _, Context) ->
    feedback({error, enoent}, Context).

feedback({ok, _}, Context) -> z_render:wire({reload, []}, Context);
feedback({error, Reason}, Context) ->
    z_render:growl_error(z_html:escape(iolist_to_binary(io_lib:format("~p", [Reason]))), Context).

save(Id, Context) ->
    Keys = [<<"title">>, <<"git_url">>, <<"website_url">>, <<"branch">>, <<"hex_package">>],
    Props = maps:from_list([{K, input(K, Context)} || K <- Keys]),
    case zotonicwww2_external_import:validate(Props) of
        ok ->
            Enabled = z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
            Values = [maps:get(K, Props) || K <- Keys] ++ [Enabled],
            case Id of
                undefined ->
                    NewId = z_db:q1("insert into zotonicwww2_external
                        (title, git_url, website_url, branch, hex_package, is_enabled, creator_id)
                        values ($1,$2,$3,$4,$5,$6,$7) returning id",
                        Values ++ [z_acl:user(Context)], Context),
                    {ok, NewId};
                N when is_integer(N) ->
                    case z_db:q1("update zotonicwww2_external set title=$1, git_url=$2,
                        website_url=$3, branch=$4, hex_package=$5, is_enabled=$6,
                        git_commit=null, status='new', error=null where id=$7 returning id",
                        Values ++ [N], Context) of
                        undefined -> {error, enoent};
                        N -> {ok, N}
                    end
            end;
        Error -> Error
    end.

input(Key, Context) ->
    case z_context:get_q(Key, Context) of
        B when is_binary(B), byte_size(B) =< 2048 -> z_string:trim(B);
        _ -> <<>>
    end.

%% @doc Hide a repository's complete documentation and stop automatic imports.
%% Lock the registry row just like the importer, so an in-flight import cannot
%% republish pages after this transaction has deprecated them.
-spec deprecate(Id, Context) -> Result when
    Id :: integer(),
    Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
deprecate(Id, Context) when is_integer(Id), Id > 0 ->
    case z_acl:is_admin(Context) of
        true ->
            case z_db:transaction(fun(Tx) ->
                case z_db:qmap_row("select id from zotonicwww2_external where id=$1 for update", [Id], Tx) of
                    {ok, _} ->
                        Source = <<"external_", (integer_to_binary(Id))/binary>>,
                        {ok, Report} = zotonicwww2_doc_import:sync_external(Source, [], <<>>, Tx),
                        1 = z_db:q("update zotonicwww2_external set is_enabled=false,
                            status='deprecated', error=null where id=$1", [Id], Tx),
                        {ok, Report};
                    {error, _} = Error -> Error
                end
            end, Context) of
                {rollback, Reason} -> {error, Reason};
                Result -> Result
            end;
        false -> {error, eacces}
    end;
deprecate(_, _) -> {error, badarg}.

-spec queue(integer(), z:context()) -> {ok, integer()} | {error, term()}.
queue(Id, Context) when is_integer(Id), Id > 0 ->
    case z_acl:is_admin(Context) of
        true -> enqueue(Id, true, Context);
        false -> {error, eacces}
    end;
queue(_, _) -> {error, badarg}.

-spec queue_all(z:context()) -> ok.
queue_all(Context) ->
    Ids = z_db:q("select id from zotonicwww2_external where is_enabled", Context),
    lists:foreach(fun({Id}) ->
        case enqueue(Id, false, Context) of
            {ok, _} -> ok;
            {error, Reason} -> ?LOG_ERROR(#{in => zotonicwww2, result => error,
                text => <<"Could not queue external documentation">>, reason => Reason, id => Id})
        end
    end, Ids),
    ok.

enqueue(Id, Force, Context) ->
    z_pivot_rsc:insert_task_after(1, zotonicwww2_external_import, task_run,
        integer_to_binary(Id), [Id, Force], Context).

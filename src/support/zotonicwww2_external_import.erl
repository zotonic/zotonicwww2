%% @doc Fetch public HTTPS repositories and import literal Erlang moduledoc.
%% Git receives argv (never shell text). Repository code is never executed.
%% Scanner atoms are confined to a short-lived VM; only safe terms return.

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

-module(zotonicwww2_external_import).
-moduledoc("
Fetch public HTTPS repositories and import literal Erlang moduledoc.
Git receives argv (never shell text). Repository code is never executed.
Scanner atoms are confined to a short-lived VM; only safe terms return.

`task_run/3` checks and imports enabled repositories. `collect_entries/3`
converts parsed documentation, configuration, observers, and dispatch rules
into a repository-scoped manifest. Pages without module documentation are
skipped. The final import checks the locked registry row for changed settings
before committing, so disabling or deprecating a repository prevents an
in-flight import from publishing its pages.
").
-export([validate/1, task_run/3, collect_entries/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec validate(map()) -> ok | {error, term()}.
validate(#{<<"title">> := Title, <<"git_url">> := Git, <<"website_url">> := Web,
           <<"branch">> := Branch, <<"hex_package">> := Hex}) ->
    case Title =/= <<>> andalso https_url(Git)
        andalso (Web =:= <<>> orelse information_url(Web))
        andalso valid_branch(Branch)
        andalso matches(Hex, <<"^[a-z0-9_]*$">>) of
        true -> ok;
        false -> {error, invalid_repository_settings}
    end;
validate(_) -> {error, invalid_repository_settings}.

https_url(Url) when is_binary(Url) ->
    try uri_string:parse(Url) of
        #{scheme := <<"https">>, host := Host} = Parts ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts)
                andalso not maps:is_key(fragment, Parts)
                andalso not maps:is_key(query, Parts)
                andalso matches(Url, <<"^[^\\s\\x00-\\x1f]+$">>);
        _ -> false
    catch _:_ -> false end;
https_url(_) -> false.

information_url(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parts when Scheme =:= <<"http">>; Scheme =:= <<"https">> ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts)
                andalso matches(Url, <<"^[^\\s\\x00-\\x1f]+$">>);
        _ -> false
    catch _:_ -> false end.

valid_branch(<<>>) -> true;
valid_branch(B) ->
    matches(B, <<"^[A-Za-z0-9][A-Za-z0-9._/-]*$">>)
        andalso binary:match(B, <<"..">>) =:= nomatch
        andalso binary:last(B) =/= $/.

matches(B, Pattern) -> re:run(B, Pattern, [{capture, none}]) =:= match.

-spec task_run(integer(), boolean(), z:context()) -> ok.
task_run(Id, Force, Context) ->
    %% The pivot task queue serializes execution. Row locking at commit time
    %% additionally prevents an edited registry record receiving stale results.
    case m_zotonicwww2_external:get(Id, Context) of
        {ok, #{<<"is_enabled">> := true} = Repo} -> run(Repo, Force, Context);
        _ -> ok
    end.

run(#{<<"id">> := Id} = Repo, Force, Context) ->
    Dir = filename:join(z_path:files_subdir_ensure(<<"external-docs">>, Context), z_ids:id()),
    try
        ok = validate(Repo),
        status(Id, <<"fetching">>, Context),
        Ref = case maps:get(<<"branch">>, Repo) of
            <<>> -> <<"HEAD">>;
            B -> <<"refs/heads/", B/binary>>
        end,
        Remote = git(["ls-remote", "--exit-code", maps:get(<<"git_url">>, Repo), Ref]),
        [Hash | _] = binary:split(z_string:trim(Remote), <<"\t">>, [global]),
        true = matches(Hash, <<"^[a-f0-9]{40,64}$">>),
        _ = z_db:q("update zotonicwww2_external set last_fetched=now() where id=$1 and is_enabled", [Id], Context),
        case not Force andalso Hash =:= maps:get(<<"git_commit">>, Repo)
            andalso maps:get(<<"status">>, Repo) =:= <<"imported">> of
            true -> status(Id, <<"imported">>, Context);
            false -> fetch_import(Repo, Dir, Context)
        end
    catch
        Class:Reason ->
            Error = z_string:truncate(iolist_to_binary(io_lib:format("~p: ~P", [Class, Reason, 12])), 2000),
            _ = z_db:q("update zotonicwww2_external set status = status || '_failed', error=$2 where id=$1 and is_enabled",
                [Id, Error], Context),
            ?LOG_WARNING(#{in => zotonicwww2, text => <<"External documentation import failed">>,
                result => error, id => Id, reason => Error})
    after
        _ = file:del_dir_r(Dir),
        _ = file:delete(<< (z_convert:to_binary(Dir))/binary, ".etf">>)
    end,
    ok.

fetch_import(#{<<"id">> := Id, <<"branch">> := Branch} = Repo, Dir, Context) ->
    BranchArgs = case Branch of <<>> -> []; _ -> ["--branch", Branch] end,
    _ = git(["clone", "--quiet", "--depth", "1", "--single-branch", "--no-local"]
        ++ BranchArgs ++ ["--", maps:get(<<"git_url">>, Repo), Dir]),
    Hash = z_string:trim(git(["-C", Dir, "rev-parse", "HEAD"])),
    _ = z_db:q("update zotonicwww2_external set last_fetched=now() where id=$1 and is_enabled", [Id], Context),
    status(Id, <<"parsing">>, Context),
    Output = <<(z_convert:to_binary(Dir))/binary, ".etf">>,
    Script = filename:join([code:priv_dir(zotonicwww2), "bin", "parse_external_docs.escript"]),
    Escript = filename:join([code:root_dir(), "bin", "escript"]),
    _ = command([Escript, Script, Dir, Output]),
    {ok, Bin} = file:read_file(Output),
    Rows = binary_to_term(Bin, [safe]),
    {Entries, Report} = collect_entries(Rows, Repo, Context),
    status(Id, <<"importing">>, Context),
    Pending = [case R of
        #{<<"name">> := _} -> R#{<<"status">> => <<"pending">>,
            <<"error">> => <<"Import pending or failed; see repository status">>};
        _ -> R
    end || R <- Report],
    _ = z_db:q("update zotonicwww2_external set report=$2 where id=$1 and is_enabled",
        [Id, z_json:encode(Pending)], Context),
    Result = z_db:transaction(fun(Ctx) ->
        {ok, Current} = z_db:qmap_row("select * from zotonicwww2_external where id=$1 for update", [Id], Ctx),
        Keys = [<<"title">>, <<"git_url">>, <<"website_url">>, <<"branch">>, <<"hex_package">>, <<"is_enabled">>],
        true = maps:with(Keys, Current) =:= maps:with(Keys, Repo),
        {ok, _} = zotonicwww2_doc_import:sync_external(source(Id), Entries, Hash, Ctx),
        FinalReport = [report_id(R, Ctx) || R <- Report],
        _ = z_db:q("update zotonicwww2_external set status='imported', error=null,
            last_imported=now(), git_commit=$2, report=$3 where id=$1",
            [Id, Hash, z_json:encode(FinalReport)], Ctx),
        ok
    end, z_acl:sudo(Context)),
    case Result of ok -> ok; Other -> error({import_transaction, Other}) end.

report_id(#{<<"name">> := Name} = R, Context) ->
    R#{<<"rsc_id">> => m_rsc:rid(Name, Context)};
report_id(R, _Context) -> R.

%% @doc Build a repository manifest and report from isolated-parser output.
%% This does not mutate resources; the caller commits the complete manifest.
-spec collect_entries([map()], map(), z:context()) -> {[map()], [map()]}.
collect_entries(Rows, Repo, Context) ->
    %% Duplicate Erlang module names are ambiguous: report all copies instead
    %% of allowing checkout traversal order to choose which page wins.
    Names = [M || #{<<"module">> := M} <- Rows],
    Counts = lists:foldl(fun(M, Acc) ->
        maps:update_with(page_name(Repo, M), fun(N) -> N + 1 end, 1, Acc)
    end, #{}, Names),
    Duplicates = [M || M <- Names, maps:get(page_name(Repo, M), Counts) > 1],
    {DispatchRows, SourceRows} = lists:partition(fun(R) -> maps:get(<<"kind">>, R, <<>>) =:= <<"dispatch">> end, Rows),
    Checked = [check_row(R, Duplicates, Context) || R <- SourceRows],
    Parsed = [R || #{<<"status">> := <<"parsed">>} = R <- Checked],
    Modules = [R || R <- Parsed, category(maps:get(<<"module">>, R)) =:= module],
    Entries = [entry(R, Repo, module_parent(R, Modules, Repo)) || R <- Parsed],
    {ModuleEntries, Components} = lists:partition(fun(E) -> maps:get(category, E) =:= module end, Entries),
    CheckedDispatch = [check_dispatch(R, Modules, Context) || R <- DispatchRows],
    DispatchEntries = [dispatch_entry(R, Repo, Parsed, Context) || #{<<"status">> := <<"parsed">>} = R <- CheckedDispatch],
    Sorted = ModuleEntries ++ Components ++ DispatchEntries,
    Report = [report_row(R, Repo) || R <- Checked ++ CheckedDispatch],
    {Sorted, Report}.

report_row(#{<<"status">> := <<"parsed">>} = Row, Repo) ->
    Name = case Row of
        #{<<"kind">> := <<"dispatch">>, <<"path">> := Path} -> dispatch_page_name(Repo, Path);
        #{<<"module">> := Module} -> page_name(Repo, Module)
    end,
    (maps:without([<<"doc">>, <<"keywords">>, <<"config">>, <<"rules">>], Row))#{
        <<"name">> => Name, <<"status">> => <<"imported">>};
report_row(Row, _) -> Row.

%% A dispatch file belongs to its application's documented module. Do not
%% attach files from undocumented applications to an unrelated umbrella app.
check_dispatch(#{<<"status">> := <<"parsed">>, <<"path">> := Path} = Row, Modules, Context) ->
    [Root, _] = binary:split(Path, <<"/priv/dispatch/">>),
    case [M || M <- Modules, source_root(maps:get(<<"path">>, M)) =:= Root] of
        [#{<<"module">> := Module}] ->
            check_row(Row#{<<"module">> => Module,
                <<"keywords">> => zotonicwww2_beam_doc:dispatch_keywords()}, [], Context);
        [] -> skipped(Row, <<"No documented module in this application">>);
        _ -> skipped(Row, <<"Ambiguous documented module in this application">>)
    end;
check_dispatch(Row, _, _) -> Row.

dispatch_entry(#{<<"path">> := Path, <<"module">> := Module, <<"rules">> := Rules,
        <<"keywords">> := Keywords}, Repo, Sources, Context) ->
    Filename = filename:basename(Path),
    LinkedRules = [dispatch_controller(R, Repo, Sources, Context) || R <- Rules],
    #{category => dispatch, kind => external, name => dispatch_page_name(Repo, Path),
      title => <<Module/binary, " dispatch rules (", Filename/binary, ")">>,
      body => zotonicwww2_beam_doc:dispatch_doc(Module, Filename, Path, LinkedRules),
      source_path => Path, source_url => maps:get(<<"git_url">>, Repo),
      module => {page, page_name(Repo, Module)}, keywords => Keywords,
      props => (external_props(Repo))#{<<"dispatch_file">> => Filename,
          <<"dispatch_rule_count">> => length(Rules)}}.

dispatch_controller(#{<<"controller">> := Controller} = Rule, Repo, Sources, Context) ->
    case lists:any(fun(R) -> maps:get(<<"module">>, R) =:= Controller end, Sources) of
        true -> Rule#{<<"controller_page">> => page_name(Repo, Controller)};
        false ->
            %% Prefer this import's manifest, then existing public core docs.
            Page = <<"doc_controller_", Controller/binary>>,
            PublicContext = z_acl:anondo(Context),
            case m_rsc:is_a(Page, controller, PublicContext)
                andalso z_acl:rsc_visible(Page, PublicContext) of
                true -> Rule#{<<"controller_page">> => Page};
                false -> Rule
            end
    end.

%% Use a distinct prefix and hash the full relative path: multiple applications
%% may each contain a file named dispatch, and names must fit rsc.name (80).
dispatch_page_name(#{<<"id">> := Id}, Path) ->
    Hash = binary:part(z_url:hex_encode_lc(crypto:hash(sha256, Path)), 0, 32),
    <<"doc_external_dispatch_", (integer_to_binary(Id))/binary, "_", Hash/binary>>.

%% A repository can contain multiple applications. Prefer the documented
%% module in the same src tree; only use a repository-wide parent if unique.
module_parent(Row, Modules, Repo) ->
    Root = source_root(maps:get(<<"path">>, Row)),
    Local = [M || M <- Modules, source_root(maps:get(<<"path">>, M)) =:= Root],
    Candidates = case Local of [] -> Modules; _ -> Local end,
    case Candidates of
        [#{<<"module">> := Module}] -> page_name(Repo, Module);
        _ -> undefined
    end.

source_root(Path) ->
    case binary:split(Path, <<"/src/">>) of
        [Root, _] -> Root;
        [_] -> filename:dirname(Path)
    end.

check_row(#{<<"status">> := <<"parsed">>, <<"module">> := M, <<"keywords">> := Keywords} = R, Dups, Context) ->
    Unknown = [K || K <- Keywords, not valid_keyword(K, Context)],
    case {lists:member(M, Dups), Unknown} of
        {false, []} -> R;
        {true, _} -> skipped(R, <<"Duplicate Erlang module name">>);
        {_, _} -> skipped(R, <<"Unknown subject keywords: ", (iolist_to_binary(lists:join(<<", ">>, Unknown)))/binary>>)
    end;
check_row(R, _, _) -> R.

valid_keyword(K, Context) ->
    case m_rsc:rid(<<"zotonic_topic_", K/binary>>, Context) of
        undefined -> false;
        Id -> m_rsc:is_a(Id, keyword, Context) andalso m_rsc:p(Id, subject_topic_slug, Context) =:= K
    end.

skipped(R, Error) ->
    (maps:without([<<"doc">>, <<"keywords">>, <<"config">>, <<"rules">>], R))#{<<"status">> => <<"skipped">>, <<"error">> => Error}.

entry(#{<<"module">> := Module, <<"doc">> := Doc, <<"path">> := Path, <<"keywords">> := Keywords} = Row, Repo, Parent) ->
    Category = category(Module),
    Entry = #{category => Category, kind => external, name => page_name(Repo, Module),
        title => Module, body => zotonicwww2_doc_link:to_html(Doc),
        source_path => Path, source_url => maps:get(<<"git_url">>, Repo), keywords => Keywords,
        props => (external_props(Repo))#{<<"doc_module_config">> => maps:get(<<"config">>, Row, [])}},
    case {Category, Parent} of
        {module, _} ->
            Observes = maps:get(<<"observes">>, Row, []),
            Entry#{observes => Observes,
                props => (maps:get(props, Entry))#{<<"doc_module_observers">> => [
                    #{<<"name">> => N, <<"page_name">> => <<"doc_notification_", N/binary>>}
                    || N <- Observes
                ]}};
        {_, undefined} -> Entry;
        _ -> Entry#{module => {page, Parent}}
    end.

external_props(Repo) ->
    #{<<"is_external_module">> => true,
      <<"external_module_id">> => maps:get(<<"id">>, Repo),
      <<"external_module_title">> => maps:get(<<"title">>, Repo),
      <<"git_url">> => maps:get(<<"git_url">>, Repo),
      <<"website_url">> => maps:get(<<"website_url">>, Repo),
      <<"hex_package">> => maps:get(<<"hex_package">>, Repo)}.

category(<<"mod_", _/binary>>) -> module;
category(<<"m_", _/binary>>) -> model;
category(<<"controller_", _/binary>>) -> controller;
category(<<"filter_", _/binary>>) -> template_filter;
category(<<"scomp_", _/binary>>) -> template_scomp;
category(<<"action_", _/binary>>) -> template_action;
category(<<"validator_", _/binary>>) -> template_validator;
category(_) -> reference.

source(Id) -> <<"external_", (integer_to_binary(Id))/binary>>.
%% rsc.name is varchar(80). Keep ordinary names readable, but never rely on
%% database truncation or resource-name normalization for uniqueness.
page_name(#{<<"id">> := Id}, Module) ->
    Prefix = <<"doc_", (source(Id))/binary, "_">>,
    Name = <<Prefix/binary, Module/binary>>,
    case byte_size(Name) =< 80 andalso z_string:to_name(Name) =:= Name of
        true -> Name;
        false ->
            Hash = binary:part(z_url:hex_encode_lc(crypto:hash(sha256, Module)), 0, 32),
            Readable = re:replace(Module, <<"[^a-z0-9_]">>, <<"_">>, [global, {return, binary}]),
            Limit = 80 - byte_size(Prefix) - 33,
            Short = binary:part(Readable, 0, min(Limit, byte_size(Readable))),
            z_string:to_name(<<Prefix/binary, Short/binary, "_", Hash/binary>>)
    end.

status(Id, Status, Context) ->
    _ = z_db:q("update zotonicwww2_external set status=$2, error=null where id=$1 and is_enabled", [Id, Status], Context),
    ok.

git(Args) ->
    command([os:find_executable("git"), "-c", "core.hooksPath=/dev/null",
        "-c", "protocol.allow=never", "-c", "protocol.https.allow=always",
        "-c", "http.followRedirects=false", "-c", "credential.helper="] ++ Args).

%% Bounded runtime and output. No shell interpolation, prompts, hooks, local
%% protocols, submodules, or repository build commands are permitted.
command(Args) ->
    {ok, Pid, OsPid} = exec:run(Args, [monitor, stdout, stderr,
        {env, [{"GIT_TERMINAL_PROMPT", "0"}, {"GIT_CONFIG_NOSYSTEM", "1"},
               {"GIT_CONFIG_GLOBAL", "/dev/null"}]}]),
    Deadline = erlang:monotonic_time(millisecond) + 120000,
    collect(Pid, OsPid, Deadline, [], [], 0).

collect(Pid, OsPid, Deadline, Out, Err, Size) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {stdout, OsPid, Data} -> collect_data(Pid, OsPid, Deadline, [Data | Out], Err, Size + byte_size(Data));
        {stderr, OsPid, Data} -> collect_data(Pid, OsPid, Deadline, Out, [Data | Err], Size + byte_size(Data));
        {'DOWN', OsPid, process, Pid, normal} -> iolist_to_binary(lists:reverse(Out));
        {'DOWN', OsPid, process, Pid, Reason} -> error({command_failed, Reason, iolist_to_binary(lists:reverse(Err))})
    after Remaining ->
        exec:stop(Pid),
        error(command_timeout)
    end.

collect_data(Pid, _OsPid, _Deadline, _Out, _Err, Size) when Size > 2097152 ->
    exec:stop(Pid), error(command_output_limit);
collect_data(Pid, OsPid, Deadline, Out, Err, Size) -> collect(Pid, OsPid, Deadline, Out, Err, Size).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
name_length_test() ->
    Repo = #{<<"id">> => 2147483647},
    Long = binary:copy(<<"a">>, 255),
    ?assertEqual(80, byte_size(page_name(Repo, Long))),
    ?assertNotEqual(page_name(Repo, <<Long/binary, "a">>), page_name(Repo, <<Long/binary, "b">>)),
    ?assertNotEqual(page_name(Repo, <<"Mod_example">>), page_name(Repo, <<"mod_example">>)),
    ?assertNotEqual(page_name(Repo, <<"mod__example">>), page_name(Repo, <<"mod_example">>)),
    ?assertEqual(page_name(Repo, Long), z_string:to_name(page_name(Repo, Long))),
    ?assertEqual(<<"doc_external_1_mod_example">>, page_name(#{<<"id">> => 1}, <<"mod_example">>)).

module_parent_test() ->
    Repo = #{<<"id">> => 1},
    A = #{<<"module">> => <<"mod_a">>, <<"path">> => <<"./apps/a/src/mod_a.erl">>},
    B = #{<<"module">> => <<"mod_b">>, <<"path">> => <<"./apps/b/src/mod_b.erl">>},
    Child = #{<<"path">> => <<"./apps/b/src/models/m_b.erl">>},
    ?assertEqual(page_name(Repo, <<"mod_b">>), module_parent(Child, [A, B], Repo)),
    ?assertEqual(undefined, module_parent(#{<<"path">> => <<"./other/src/x.erl">>}, [A, B], Repo)).

dispatch_entry_test() ->
    Repo = #{<<"id">> => 2147483647, <<"title">> => <<"Example">>,
        <<"git_url">> => <<"https://example.com/repo">>, <<"website_url">> => <<>>, <<"hex_package">> => <<>>},
    Path = <<"./apps/example/priv/dispatch/dispatch">>,
    Rules = [#{<<"name">> => <<"page">>, <<"path">> => <<"/page/:id">>,
        <<"controller">> => <<"controller_example">>, <<"options">> => <<"<script>bad</script>">>}],
    Entry = dispatch_entry(#{<<"path">> => Path, <<"module">> => <<"mod_example">>,
        <<"rules">> => Rules, <<"keywords">> => []}, Repo,
        [#{<<"module">> => <<"controller_example">>}], undefined),
    ?assertEqual(dispatch, maps:get(category, Entry)),
    ?assertEqual({page, page_name(Repo, <<"mod_example">>)}, maps:get(module, Entry)),
    Body = maps:get(body, Entry),
    ?assertNotEqual(nomatch, binary:match(Body, <<"/id/doc_external_2147483647_controller_example">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"&lt;script&gt;bad&lt;/script&gt;">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"<script>">>)),
    ?assert(byte_size(maps:get(name, Entry)) =< 80),
    ?assertNotEqual(dispatch_page_name(Repo, Path), dispatch_page_name(Repo, <<"./apps/other/priv/dispatch/dispatch">>)),
    ?assertNotEqual(dispatch_page_name(Repo, Path), dispatch_page_name(Repo#{<<"id">> => 1}, Path)),
    ?assertMatch(#{<<"status">> := <<"skipped">>}, check_dispatch(
        #{<<"status">> => <<"parsed">>, <<"path">> => Path, <<"rules">> => Rules}, [], undefined)).

validation_test() ->
    Base = #{<<"title">> => <<"Example">>, <<"git_url">> => <<"https://github.com/example/mod_example.git">>,
        <<"website_url">> => <<>>, <<"branch">> => <<>>, <<"hex_package">> => <<"mod_example">>},
    ?assertEqual(ok, validate(Base)),
    ?assertEqual(ok, validate(Base#{<<"website_url">> => <<"http://example.com/docs?q=module#install">>})),
    ?assertMatch({error, _}, validate(Base#{<<"website_url">> => <<"javascript:alert(1)">>})),
    lists:foreach(fun(Url) -> ?assertMatch({error, _}, validate(Base#{<<"git_url">> => Url})) end,
        [<<"file:///etc">>, <<"ext::evil">>, <<"https://user:secret@example.com/repo">>, <<"https://example.com/\nrepo">>]),
    ?assertMatch({error, _}, validate(Base#{<<"branch">> => <<"--upload-pack=evil">>})),
    ?assertNotEqual(page_name(#{<<"id">> => 1}, <<"mod_example">>), page_name(#{<<"id">> => 2}, <<"mod_example">>)).
-endif.

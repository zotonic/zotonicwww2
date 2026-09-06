%% @doc Load the documentation from a beam file.
%% This file has code adapted from code:get_doc/1, as we don't want to load
%% the beam file into the VM when fetching the docs from the beam file.
%%
%% Controlled subject keywords are added as EEP-48 metadata next to the
%% documentation text. Both module and callback documentation use the same
%% list of canonical slugs:
%%
%% `-moduledoc(#{zotonic_keywords => ["module_management", "configure"]}).`
%%
%% `-doc(#{zotonic_keywords => ["media_management", "upload"]}).`
%% @end

-module(zotonicwww2_beam_doc).

-include_lib("kernel/include/eep48.hrl").
% -include_lib("zotonic_core/include/zotonic.hrl").

-export([
    import_docs/1,
    collect_entries/1,

    get_doc/1,
    get_modules_doc/1,
    get_dispatch_docs/1,
    get_filters_doc/1,
    get_models_doc/1,
    get_controllers_doc/1,
    get_scomps_doc/1,
    get_validators_doc/1,
    get_actions_doc/1,
    get_releases_doc/1,
    get_tags_doc/1,
    get_notifications_doc/1
]).


%% @doc Import a complete documentation manifest. Missing tracked source pages
%% are only deprecated after all collectors and upserts have succeeded.
-spec import_docs(z:context()) -> {ok, map()} | {error, term()}.
import_docs(Context) ->
    case m_zotonicwww2_git:hash(Context) of
        {ok, Commit} ->
            try collect_entries(Context) of
                Entries when is_list(Entries), Entries =/= [] ->
                    zotonicwww2_doc_import:sync(Entries, Commit, Context);
                [] ->
                    {error, empty_manifest}
            catch
                Class:Reason:Stack ->
                    {error, {manifest, Class, Reason, Stack}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Collect normalized entries in dependency order. Module resources are
%% stored first so component-to-module edges can always be created.
-spec collect_entries(z:context()) -> [map()].
collect_entries(Context) ->
    module_entries(Context)
    ++ dispatch_entries(Context)
    ++ controller_entries(Context)
    ++ filter_entries(Context)
    ++ scomp_entries(Context)
    ++ model_entries(Context)
    ++ action_entries(Context)
    ++ validator_entries(Context)
    ++ release_entries(Context)
    ++ tag_entries(Context)
    ++ notification_entries(Context).

module_entries(Context) ->
    maps:fold(
        fun(App, #{doc := Body, keywords := Keywords, path := Path}, Acc) ->
            Module = app2mod(App),
            Entry = entry(module, module_page_name(Module), module_title(Module), Body, Path),
            [Entry#{
                keywords => Keywords,
                props => #{<<"erlang_app">> => App}
            } | Acc]
        end,
        [],
        get_modules_doc(Context)).

dispatch_entries(Context) ->
    maps:fold(
        fun(Name, #{
                app := App,
                doc := Body,
                filename := Filename,
                module := Module,
                path := Path,
                rules := Rules,
                source_url := SourceUrl
            }, Acc) ->
            Entry = entry(dispatch, dispatch, Name, dispatch_title(Module, Filename), Body, Path),
            [Entry#{
                keywords => dispatch_keywords(),
                module => Module,
                source_url => SourceUrl,
                props => #{
                    <<"erlang_app">> => App,
                    <<"dispatch_file">> => Filename,
                    <<"dispatch_rule_count">> => length(Rules),
                    <<"summary">> => iolist_to_binary([
                        <<"URL dispatch rules defined by ">>, Module,
                        <<" in ">>, Path, <<".">>
                    ])
                }
            } | Acc]
        end,
        [],
        get_dispatch_docs(Context)).

controller_entries(Context) ->
    component_entries(
        get_controllers_doc(Context),
        controller,
        controller,
        fun(<<"controller_", Controller/binary>>, _Module) ->
            {<<"doc_controller_controller_", Controller/binary>>, <<"controller_", Controller/binary>>}
        end).

filter_entries(Context) ->
    component_entries(
        get_filters_doc(Context),
        template_filter,
        filter,
        fun(<<"filter_", Filter/binary>>, _Module) ->
            {<<"doc_template_filter_filter_", Filter/binary>>, Filter}
        end).

scomp_entries(Context) ->
    component_entries(
        get_scomps_doc(Context),
        template_scomp,
        scomp,
        fun(Name, Module) ->
            Scomp = remove_mod(Name, Module),
            {<<"doc_template_scomp_scomp_", Scomp/binary>>, Scomp}
        end).

model_entries(Context) ->
    component_entries(
        get_models_doc(Context),
        model,
        model,
        fun(<<"m_", Model/binary>>, _Module) ->
            {<<"doc_model_model_", Model/binary>>, Model}
        end).

action_entries(Context) ->
    component_entries(
        get_actions_doc(Context),
        template_action,
        action,
        fun(Name, Module) ->
            Action = remove_mod(Name, Module),
            {<<"doc_template_action_action_", Action/binary>>, Action}
        end).

validator_entries(Context) ->
    component_entries(
        get_validators_doc(Context),
        template_validator,
        validator,
        fun(Name, Module) ->
            Validator = remove_mod(Name, Module),
            {<<"doc_template_validator_validator_", Validator/binary>>, Validator}
        end).

component_entries(Docs, Category, Kind, NameFun) ->
    maps:fold(
        fun(Name, #{app := App, doc := Body, keywords := Keywords, module := Module, path := Path}, Acc) ->
            {PageName, Title} = NameFun(Name, Module),
            Entry = entry(Category, Kind, PageName, Title, Body, Path),
            [Entry#{
                keywords => Keywords,
                module => Module,
                props => #{<<"erlang_app">> => App, <<"erlang_module">> => Name}
            } | Acc]
        end,
        [],
        Docs).

release_entries(Context) ->
    maps:fold(
        fun(Version, #{doc := Body, path := Path, org_pubdate := OrgPubDate}, Acc) ->
            Name = <<"doc_releasenotes_rel_", Version/binary>>,
            Entry = entry(releasenotes, release, Name, Version, Body, Path),
            [Entry#{props => #{
                <<"version">> => Version,
                <<"org_pubdate">> => OrgPubDate,
                <<"tz">> => <<"UTC">>
            }} | Acc]
        end,
        [],
        get_releases_doc(Context)).

tag_entries(Context) ->
    maps:fold(
        fun(Tag, #{doc := Body, keywords := Keywords, path := Path}, Acc) ->
            Name = <<"doc_template_tag_tag_", Tag/binary>>,
            Entry = entry(template_tag, template_tag, Name, Tag, Body, Path),
            [Entry#{keywords => Keywords} | Acc]
        end,
        [],
        get_tags_doc(Context)).

notification_entries(Context) ->
    maps:fold(
        fun(_Name, Doc, Acc) ->
            #{
                notification := Notification,
                sig := Sig,
                arity := Arity,
                callbacks := Callbacks,
                record := Fields,
                doc := Body,
                keywords := Keywords,
                path := Path
            } = Doc,
            Name = <<"doc_notification_", Notification/binary>>,
            Entry = entry(notification, notification, Name, Sig, Body, Path),
            [Entry#{
                keywords => Keywords,
                props => #{
                    <<"notification">> => #{
                        <<"name">> => Notification,
                        <<"arity">> => Arity,
                        <<"callbacks">> => Callbacks,
                        <<"record">> => Fields
                    }
                }
            } | Acc]
        end,
        [],
        get_notifications_doc(Context)).

entry(Category, Name, Title, Body, Path) ->
    entry(Category, Category, Name, Title, Body, Path).

entry(Category, Kind, Name, Title, Body, Path) ->
    #{
        category => Category,
        kind => Kind,
        name => Name,
        title => Title,
        body => Body,
        source_path => Path
    }.


%% @doc List the modules for which we expect documentation.
-spec get_modules_doc(z:context()) -> #{ binary() => map() }.
get_modules_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Apps = filelib:wildcard(unicode:characters_to_list(filename:join(AppDir, "*"))),
    lists:foldl(
        fun(Dir, Acc) ->
            case filelib:is_dir(Dir) of
                true ->
                    AppName = unicode:characters_to_binary(filename:basename(Dir)),
                    case beam_for_docs(AppName) of
                        false ->
                            Acc;
                        DocFilename ->
                            Beamfile = filename:join([ GitDir, "_build", "default", "lib", AppName, "ebin", DocFilename ]),
                            SourceName = filename:rootname(DocFilename, <<".beam">>),
                            SourcePath = filename:join([ <<"apps">>, AppName, <<"src">>, <<SourceName/binary, ".erl">> ]),
                            case get_doc(Beamfile) of
                                {ok, #docs_v1{
                                    module_doc = #{ <<"en">> := Doc },
                                    metadata = Metadata
                                }} ->
                                    Acc#{
                                        AppName => #{
                                            doc => z_markdown:to_html(Doc),
                                            keywords => zotonic_keywords(Metadata),
                                            path => SourcePath
                                        }
                                    };
                                _ ->
                                    % A module page is still useful for grouping all
                                    % components when the application has no moduledoc.
                                    Acc#{
                                        AppName => #{
                                            doc => <<>>,
                                            keywords => [],
                                            path => SourcePath
                                        }
                                    }
                            end
                    end;
                false ->
                    Acc
            end
        end,
        #{},
        Apps).

%% @doc Read every dispatch file from the Zotonic applications. One generated
%% documentation entry is kept per source file, using the legacy resource name
%% so imports replace converted RST pages instead of creating duplicates.
-spec get_dispatch_docs(z:context()) -> #{ binary() => map() }.
get_dispatch_docs(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(
        unicode:characters_to_list(filename:join([AppDir, "*", "priv", "dispatch", "*"]))),
    lists:foldl(
        fun(File, Acc) ->
            case is_dispatch_source(File) of
                true ->
                    App = dispatch_app_name(File),
                    Module = app2mod(App),
                    Filename = unicode:characters_to_binary(filename:basename(File)),
                    Path = relative_path(File, GitDir),
                    Rules = read_dispatch_rules(File),
                    Name = dispatch_page_name(Module, Filename),
                    Acc#{Name => #{
                        app => App,
                        doc => dispatch_doc(Module, Filename, Path, Rules),
                        filename => Filename,
                        module => Module,
                        path => Path,
                        rules => Rules,
                        source_url => master_source_url(Path)
                    }};
                false ->
                    Acc
            end
        end,
        #{},
        Files).

is_dispatch_source(File) ->
    filelib:is_regular(File)
        andalso not zotonic_filewatcher_handler:is_file_blocked(File)
        andalso not is_hidden_file(filename:basename(File)).

is_hidden_file([$. | _]) -> true;
is_hidden_file(<<$., _/binary>>) -> true;
is_hidden_file(_) -> false.

read_dispatch_rules(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Rules = lists:flatten(Terms),
            case lists:all(fun is_dispatch_rule/1, Rules) of
                true -> Rules;
                false -> erlang:error({invalid_dispatch_rules, File})
            end;
        {error, Reason} ->
            erlang:error({invalid_dispatch_file, File, Reason})
    end.

is_dispatch_rule({Name, _Path, Controller, Options}) ->
    is_atom(Name) andalso is_atom(Controller) andalso is_list(Options);
is_dispatch_rule(_) ->
    false.

dispatch_doc(Module, Filename, Path, Rules) ->
    Intro = [
        <<"<p>This file defines the URL dispatch rules for <code>">>,
        z_html:escape(Module),
        <<"</code>. Rules are matched from top to bottom; the first matching rule handles the request.</p>">>,
        <<"<p>Source: <code>">>, z_html:escape(Path), <<"</code></p>">>
    ],
    Table = [
        <<"<div class=\"table-responsive\"><table><thead><tr>">>,
        <<"<th>Dispatch name</th><th>Path</th><th>Controller</th><th>Options</th>">>,
        <<"</tr></thead><tbody>">>,
        [ dispatch_rule_row(Rule) || Rule <- Rules ],
        <<"</tbody></table></div>">>
    ],
    iolist_to_binary([Intro, dispatch_file_note(Filename), Table]).

dispatch_file_note(<<"dispatch">>) ->
    [];
dispatch_file_note(Filename) ->
    [
        <<"<p>This set is stored in the <code>">>,
        z_html:escape(Filename),
        <<"</code> dispatch file.</p>">>
    ].

dispatch_rule_row({Name, Path, Controller, Options}) ->
    [
        <<"<tr><td><code>">>, z_html:escape(atom_to_binary(Name)), <<"</code></td>">>,
        <<"<td><code>">>, z_html:escape(dispatch_path(Path)), <<"</code></td>">>,
        <<"<td><code>">>, z_html:escape(atom_to_binary(Controller)), <<"</code></td>">>,
        <<"<td><code>">>, z_html:escape(term_text(Options)), <<"</code></td></tr>">>
    ].

dispatch_path([]) ->
    <<"/">>;
dispatch_path([C | _] = Path) when is_integer(C) ->
    unicode:characters_to_binary(Path);
dispatch_path(Path) when is_list(Path) ->
    Segments = [ dispatch_path_segment(Segment) || Segment <- Path ],
    iolist_to_binary([<<"/">>, lists:join(<<"/">>, Segments)]);
dispatch_path(Path) ->
    term_text(Path).

dispatch_path_segment('*') ->
    <<"*">>;
dispatch_path_segment(Segment) when is_atom(Segment) ->
    <<":", (atom_to_binary(Segment))/binary>>;
dispatch_path_segment(Segment) when is_binary(Segment) ->
    Segment;
dispatch_path_segment(Segment) when is_list(Segment) ->
    unicode:characters_to_binary(Segment);
dispatch_path_segment({Variable, Match}) when is_atom(Variable) ->
    iolist_to_binary([<<":">>, atom_to_binary(Variable), <<"=">>, term_text(Match)]);
dispatch_path_segment(Segment) ->
    term_text(Segment).

term_text(Term) ->
    iolist_to_binary(io_lib:format("~tp", [Term])).

dispatch_keywords() ->
    [
        <<"reference">>,
        <<"backend_developer">>,
        <<"routing_and_redirects">>,
        <<"dispatch_rule">>,
        <<"url">>
    ].

dispatch_app_name(Filename) ->
    Filename1 = unicode:characters_to_binary(Filename),
    [Dir, _] = binary:split(Filename1, <<"/priv/dispatch/">>),
    filename:basename(Dir).

dispatch_page_name(Module, Filename) ->
    z_string:to_name(
        <<"doc_dispatch_dispatch_", Module/binary, "_", Filename/binary>>).

dispatch_title(Module, <<"dispatch">>) ->
    <<Module/binary, " dispatch rules">>;
dispatch_title(Module, Filename) ->
    <<Module/binary, " dispatch rules (", Filename/binary, ")">>.

master_source_url(Path) ->
    <<"https://github.com/zotonic/zotonic/blob/master/", Path/binary>>.

%% @doc List the filters for which we expect documentation.
-spec get_filters_doc(z:context()) -> #{ binary() => map() }.
get_filters_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "filters", "filter_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the models for which we expect documentation.
-spec get_models_doc(z:context()) -> #{ binary() => map() }.
get_models_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "models", "m_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the scomps for which we expect documentation.
-spec get_scomps_doc(z:context()) -> #{ binary() => map() }.
get_scomps_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "scomps", "scomp_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the actions for which we expect documentation.
-spec get_actions_doc(z:context()) -> #{ binary() => map() }.
get_actions_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "actions", "action_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the validators for which we expect documentation.
-spec get_validators_doc(z:context()) -> #{ binary() => map() }.
get_validators_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "validators", "validator_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the controllers for which we expect documentation.
-spec get_controllers_doc(z:context()) -> #{ binary() => map() }.
get_controllers_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "controllers", "controller_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the releases and their documentation.
-spec get_releases_doc(z:context()) -> #{ binary() => map() }.
get_releases_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([GitDir, "doc", "release-notes", "*.md"]))),
    lists:foldl(
        fun(File, Acc) ->
            Version = unicode:characters_to_binary(filename:rootname(filename:basename(File))),
            {ok, Data} = file:read_file(File),
            Acc#{Version => #{
                doc => z_markdown:to_html(Data),
                path => relative_path(File, GitDir),
                org_pubdate => zotonicwww2_release_notes:org_pubdate(Data)
            }}
        end,
        #{},
        Files).

%% @doc List the releases and their documentation.
-spec get_tags_doc(z:context()) -> #{ binary() => binary() }.
get_tags_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([GitDir, "doc", "template-tags", "*.md"]))),
    lists:foldl(
        fun(File, Acc) ->
            Tag = unicode:characters_to_binary(filename:rootname(filename:basename(File))),
            {ok, Data} = file:read_file(File),
            {ok, #{
                front_matter := FrontMatter,
                content := Html
            }} = z_markdown:to_html_document(Data),
            Acc#{Tag => #{
                doc => Html,
                keywords => markdown_keywords(FrontMatter),
                path => relative_path(File, GitDir)
            }}
        end,
        #{},
        Files).

%% @doc Inspect the observer behaviour beam file and extract one documentation
%% entry per logical notification. Regular and PID observer callbacks are kept
%% as variations of that entry.
-spec get_notifications_doc(z:context()) -> #{ binary() => map() }.
get_notifications_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    BeamFile = filename:join([ GitDir, "_build", "default", "lib", "zotonic_core", "ebin", "zotonic_observer.beam" ]),
    {ok, #docs_v1{
        docs = Docs
    }} = get_doc(BeamFile),
    notification_docs(Docs).

notification_docs(Docs) ->
    lists:foldl(
        fun
            ({{callback, FunctionName, Arity}, _LineCol, [Sig], #{ <<"en">> := Doc }, Metadata}, Acc) ->
                case notification_callback(FunctionName) of
                    {Variation, Notification} ->
                        NotificationDoc = #{
                            notification => Notification,
                            variation => Variation,
                            sig => Sig,
                            name => FunctionName,
                            arity => Arity,
                            callbacks => [callback_metadata(Variation, FunctionName, Arity, Sig)],
                            doc => z_markdown:to_html(Doc),
                            keywords => zotonic_keywords(Metadata),
                            path => <<"apps/zotonic_core/src/behaviours/zotonic_observer.erl">>,
                            record => record_fields(Notification)
                        },
                        maps:update_with(
                            Notification,
                            fun(Existing) -> merge_notification_doc(NotificationDoc, Existing) end,
                            NotificationDoc,
                            Acc);
                    false ->
                        Acc
                end;
            (_, Acc) ->
                Acc
        end,
        #{},
        Docs).

%% A notification has a regular observer callback and usually a PID observer
%% callback with an additional PID argument. Both callbacks document the same
%% logical notification and must therefore result in a single resource.
notification_callback(FunctionName) ->
    case atom_to_binary(FunctionName) of
        <<"observe_", Notification/binary>> -> {observe, Notification};
        <<"pid_observe_", Notification/binary>> -> {pid_observe, Notification};
        _ -> false
    end.

callback_metadata(Variation, FunctionName, Arity, Sig) ->
    #{
        <<"variation">> => atom_to_binary(Variation),
        <<"name">> => atom_to_binary(FunctionName),
        <<"arity">> => Arity,
        <<"signature">> => Sig
    }.

merge_notification_doc(#{
        variation := Variation,
        callbacks := NewCallbacks
    } = New, #{callbacks := ExistingCallbacks} = Existing) ->
    Callbacks = sort_callbacks(NewCallbacks ++ ExistingCallbacks),
    case Variation of
        observe -> New#{callbacks => Callbacks};
        pid_observe -> Existing#{callbacks => Callbacks}
    end.

sort_callbacks(Callbacks) ->
    lists:sort(
        fun(A, B) -> callback_sort_key(A) =< callback_sort_key(B) end,
        lists:uniq(Callbacks)).

callback_sort_key(#{<<"variation">> := <<"observe">>, <<"signature">> := Sig}) ->
    {0, Sig};
callback_sort_key(#{<<"signature">> := Sig}) ->
    {1, Sig}.


get_docs(Files, Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    lists:foldl(
        fun(File, Acc) ->
            AppName = app_name(File),
            Name = unicode:characters_to_binary(filename:rootname(filename:basename(File))),
            DocFilename = unicode:characters_to_binary([Name, ".beam"]),
            Beamfile = filename:join([ GitDir, "_build", "default", "lib", AppName, "ebin", DocFilename ]),
            case get_doc(Beamfile) of
                {ok, #docs_v1{
                    module_doc = #{ <<"en">> := Doc },
                    metadata = Metadata
                }} ->
                   Acc#{
                        Name => #{
                            doc => z_markdown:to_html(Doc),
                            keywords => zotonic_keywords(Metadata),
                            app => AppName,
                            module => app2mod(AppName),
                            path => relative_path(File, GitDir)
                        }
                    };
                _ ->
                    Acc
            end
        end,
        #{},
        Files).


%% @doc Extract the controlled-vocabulary slugs from EEP-48 documentation
%% metadata. Source strings are normalized to binaries for the import manifest.
%% Malformed metadata is rejected so misspelled or structurally invalid keyword
%% declarations do not silently disappear from the generated documentation.
zotonic_keywords(Metadata) when is_map(Metadata) ->
    case maps:get(zotonic_keywords, Metadata, []) of
        Keywords when is_list(Keywords) ->
            unique_keywords([ normalize_keyword(Keyword) || Keyword <- Keywords ]);
        Invalid ->
            erlang:error({invalid_zotonic_keywords, Invalid})
    end.

%% @doc Extract generic keyword slugs from Markdown YAML front matter.
markdown_keywords(undefined) ->
    [];
markdown_keywords(#{format := yaml, source := Source}) ->
    Metadata = decode_yaml_front_matter(Source),
    case maps:get(<<"keywords">>, Metadata, []) of
        Keywords when is_list(Keywords) ->
            unique_keywords([normalize_keyword(Keyword) || Keyword <- Keywords]);
        Invalid ->
            erlang:error({invalid_markdown_keywords, Invalid})
    end.

decode_yaml_front_matter(Source) ->
    ok = ensure_yamerl_loaded(),
    Documents = try
        yamerl_constr:string(Source, [
            str_node_as_binary,
            {map_node_format, map}
        ])
    catch
        Class:Reason ->
            erlang:error({invalid_markdown_front_matter, {Class, Reason}})
    end,
    case Documents of
        [] -> #{};
        [Metadata] when is_map(Metadata) -> Metadata;
        Invalid -> erlang:error({invalid_markdown_front_matter, Invalid})
    end.

ensure_yamerl_loaded() ->
    case application:load(yamerl) of
        ok -> ok;
        {error, {already_loaded, yamerl}} -> ok
    end.

normalize_keyword(Keyword) when is_binary(Keyword), Keyword =/= <<>> ->
    Keyword;
normalize_keyword(Keyword) when is_list(Keyword), Keyword =/= [] ->
    try unicode:characters_to_binary(Keyword) of
        KeywordBinary when is_binary(KeywordBinary), KeywordBinary =/= <<>> ->
            KeywordBinary;
        _ ->
            erlang:error({invalid_zotonic_keyword, Keyword})
    catch
        error:badarg -> erlang:error({invalid_zotonic_keyword, Keyword})
    end;
normalize_keyword(Invalid) ->
    erlang:error({invalid_zotonic_keyword, Invalid}).

unique_keywords(Keywords) ->
    unique_keywords(Keywords, sets:new(), []).

unique_keywords([], _Seen, Acc) ->
    lists:reverse(Acc);
unique_keywords([Keyword | Rest], Seen, Acc) ->
    case sets:is_element(Keyword, Seen) of
        true -> unique_keywords(Rest, Seen, Acc);
        false -> unique_keywords(Rest, sets:add_element(Keyword, Seen), [ Keyword | Acc ])
    end.


app_name(Filename) ->
    Filename1 = unicode:characters_to_binary(Filename),
    [Dir, _] = binary:split(Filename1, <<"/src/">>),
    filename:basename(Dir).

app2mod(<<"zotonic_mod_", M/binary>>) -> <<"mod_", M/binary>>;
app2mod(App) -> App.

module_page_name(<<"zotonic_core">>) -> <<"doc_core">>;
module_page_name(Module) -> <<"doc_module_", Module/binary>>.

module_title(<<"zotonic_core">>) -> <<"Zotonic Core">>;
module_title(Module) -> Module.

relative_path(Filename, GitDir) ->
    FilenameBin = unicode:characters_to_binary(Filename),
    GitDirBin = unicode:characters_to_binary(GitDir),
    Prefix = <<GitDirBin/binary, "/">>,
    PrefixSize = byte_size(Prefix),
    case FilenameBin of
        <<Prefix:PrefixSize/binary, Relative/binary>> -> Relative;
        _ -> FilenameBin
    end.

record_fields(Notification) ->
    try binary_to_existing_atom(Notification) of
        Rec ->
            case m_development:lookup_record(Rec) of
                {Rec, Fields} -> Fields;
                none -> undefined
            end
    catch
        error:badarg -> undefined
    end.

%% @doc Actions, scomps and validators have the module name in their name.
%% Remove the module name to get to the base action/scomp/validator name, as
%% used in the templates.
remove_mod(<<"validator_", Name/binary>>, <<"mod_", Mod/binary>>) ->
    [ <<>>, <<"_", Base/binary>> ] = binary:split(Name, Mod),
    Base;
remove_mod(<<"action_", Name/binary>>, <<"mod_", Mod/binary>>) ->
    [ <<>>, <<"_", Base/binary>> ] = binary:split(Name, Mod),
    Base;
remove_mod(<<"scomp_", Name/binary>>, <<"mod_", Mod/binary>>) ->
    [ <<>>, <<"_", Base/binary>> ] = binary:split(Name, Mod),
    Base.

beam_for_docs(<<"zotonic_mod_", Mod/binary>>) ->
    <<"mod_", Mod/binary, ".beam">>;
beam_for_docs(<<"zotonic_", _/binary>> = Mod) ->
    <<Mod/binary, ".beam">>;
beam_for_docs(_) ->
    false.

-spec get_doc(Filename) -> {ok, Res} | {error, Reason} when
      Filename :: file:filename_all(),
      Res :: #docs_v1{},
      Reason :: non_existing | missing | file:posix().
get_doc(Filename) ->
    case beam_lib:chunks(unicode:characters_to_list(Filename), ["Docs"]) of
        {error, beam_lib, {missing_chunk,_,_}} ->
            {error, missing};
        {error, beam_lib, {file_error,_Filename,Err}} ->
           {error, Err};
        {error, beam_lib, {not_a_beam_file, _Filename}} ->
            {error, non_existing};
        {ok, {_Mod, [{"Docs",Bin}]}} ->
            {ok, binary_to_term(Bin)}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_names_test() ->
    ?assertEqual(<<"mod_base">>, app2mod(<<"zotonic_mod_base">>)),
    ?assertEqual(<<"zotonic_core">>, app2mod(<<"zotonic_core">>)),
    ?assertEqual(<<"doc_core">>, module_page_name(<<"zotonic_core">>)),
    ?assertEqual(<<"doc_module_mod_base">>, module_page_name(<<"mod_base">>)).

dispatch_names_test() ->
    ?assertEqual(
        <<"doc_dispatch_dispatch_mod_acl_user_groups_dispatch">>,
        dispatch_page_name(<<"mod_acl_user_groups">>, <<"dispatch">>)),
    ?assertEqual(
        <<"doc_dispatch_dispatch_mod_base_dispatch_cotonic">>,
        dispatch_page_name(<<"mod_base">>, <<"dispatch-cotonic">>)),
    ?assertEqual(
        <<"mod_acl_user_groups dispatch rules">>,
        dispatch_title(<<"mod_acl_user_groups">>, <<"dispatch">>)),
    ?assertEqual(
        <<"mod_base dispatch rules (dispatch-cotonic)">>,
        dispatch_title(<<"mod_base">>, <<"dispatch-cotonic">>)).

dispatch_doc_test() ->
    Body = dispatch_doc(
        <<"mod_example">>,
        <<"dispatch">>,
        <<"apps/zotonic_mod_example/priv/dispatch/dispatch">>,
        [
            {example, ["item", id], controller_example, [{template, <<"page.tpl">>}]},
            {all, ["item", '*'], controller_example, []}
        ]),
    ?assertNotEqual(nomatch, binary:match(Body, <<"mod_example">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"/item/:id">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"/item/*">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"controller_example">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"page.tpl">>)).

dispatch_source_url_test() ->
    ?assertEqual(
        <<"https://github.com/zotonic/zotonic/blob/master/apps/zotonic_mod_base/priv/dispatch/dispatch">>,
        master_source_url(<<"apps/zotonic_mod_base/priv/dispatch/dispatch">>)).

component_name_test() ->
    ?assertEqual(<<"wire">>, remove_mod(<<"action_base_wire">>, <<"mod_base">>)),
    ?assertEqual(<<"menu">>, remove_mod(<<"scomp_base_menu">>, <<"mod_base">>)).

relative_path_test() ->
    ?assertEqual(
        <<"apps/zotonic_mod_base/src/mod_base.erl">>,
        relative_path(
            <<"/tmp/zotonic/apps/zotonic_mod_base/src/mod_base.erl">>,
            <<"/tmp/zotonic">>)).

zotonic_keywords_test() ->
    ?assertEqual([], zotonic_keywords(#{})),
    ?assertEqual(
        [ <<"search_and_discovery">>, <<"query">> ],
        zotonic_keywords(#{
            zotonic_keywords => [
                "search_and_discovery",
                <<"query">>,
                "search_and_discovery"
            ]
        })),
    ?assertError(
        {invalid_zotonic_keywords, <<"query">>},
        zotonic_keywords(#{zotonic_keywords => <<"query">>})),
    ?assertError(
        {invalid_zotonic_keyword, <<>>},
        zotonic_keywords(#{zotonic_keywords => [<<>>]})).

markdown_keywords_test() ->
    ?assertEqual([], markdown_keywords(undefined)),
    ?assertEqual(
        [<<"cache">>, <<"render">>],
        markdown_keywords(#{
            format => yaml,
            source => <<"keywords:\n  - cache\n  - render\n  - cache">>
        })),
    ?assertError(
        {invalid_markdown_keywords, <<"cache">>},
        markdown_keywords(#{
            format => yaml,
            source => <<"keywords: cache">>
        })).

notification_keywords_are_per_notification_test() ->
    Docs = [
        {
            {callback, observe_search_query, 2},
            10,
            [<<"observe_search_query(Notification, Context)">>],
            #{<<"en">> => <<"Search query notification.">>},
            #{zotonic_keywords => ["search_and_discovery", "query"]}
        },
        {
            {callback, observe_media_upload, 2},
            20,
            [<<"observe_media_upload(Notification, Context)">>],
            #{<<"en">> => <<"Media upload notification.">>},
            #{zotonic_keywords => ["media", "upload"]}
        }
    ],
    Notifications = notification_docs(Docs),
    ?assertMatch(
        #{keywords := [ <<"search_and_discovery">>, <<"query">> ]},
        maps:get(<<"search_query">>, Notifications)),
    ?assertMatch(
        #{keywords := [ <<"media">>, <<"upload">> ]},
        maps:get(<<"media_upload">>, Notifications)).

notification_callback_variations_are_merged_test() ->
    Observe = {
        {callback, observe_search_query, 2},
        10,
        [<<"observe_search_query(Notification, Context)">>],
        #{<<"en">> => <<"Search query notification.">>},
        #{zotonic_keywords => ["search_and_discovery", "query"]}
    },
    PidObserve = {
        {callback, pid_observe_search_query, 3},
        20,
        [<<"pid_observe_search_query(Pid, Notification, Context)">>],
        #{<<"en">> => <<"PID search query notification.">>},
        #{zotonic_keywords => ["search_and_discovery"]}
    },
    Forward = notification_docs([Observe, PidObserve]),
    Reverse = notification_docs([PidObserve, Observe]),
    ?assertEqual(Forward, Reverse),
    ?assertEqual(1, map_size(Forward)),
    Notification = maps:get(<<"search_query">>, Forward),
    ?assertMatch(
        #{
            variation := observe,
            name := observe_search_query,
            arity := 2,
            sig := <<"observe_search_query(Notification, Context)">>,
            keywords := [<<"search_and_discovery">>, <<"query">>]
        },
        Notification),
    ?assertEqual(
        [
            #{
                <<"variation">> => <<"observe">>,
                <<"name">> => <<"observe_search_query">>,
                <<"arity">> => 2,
                <<"signature">> => <<"observe_search_query(Notification, Context)">>
            },
            #{
                <<"variation">> => <<"pid_observe">>,
                <<"name">> => <<"pid_observe_search_query">>,
                <<"arity">> => 3,
                <<"signature">> => <<"pid_observe_search_query(Pid, Notification, Context)">>
            }
        ],
        maps:get(callbacks, Notification)).

notification_pid_callback_is_used_as_fallback_test() ->
    Docs = [
        {
            {callback, pid_observe_background_job, 3},
            10,
            [<<"pid_observe_background_job(Pid, Notification, Context)">>],
            #{<<"en">> => <<"Background job notification.">>},
            #{zotonic_keywords => ["scheduled_and_background_work"]}
        }
    ],
    ?assertMatch(
        #{
            notification := <<"background_job">>,
            variation := pid_observe,
            name := pid_observe_background_job,
            arity := 3
        },
        maps:get(<<"background_job">>, notification_docs(Docs))).

compiled_notifications_are_unique_test() ->
    {ok, #docs_v1{docs = Docs}} = get_doc(code:which(zotonic_observer)),
    NotificationNames = lists:filtermap(
        fun
            ({{callback, FunctionName, _Arity}, _LineCol, _Sigs, #{<<"en">> := _Doc}, _Metadata}) ->
                case notification_callback(FunctionName) of
                    {_Variation, Notification} -> {true, Notification};
                    false -> false
                end;
            (_) ->
                false
        end,
        Docs),
    Notifications = notification_docs(Docs),
    ?assertEqual(length(lists:usort(NotificationNames)), map_size(Notifications)).

-endif.

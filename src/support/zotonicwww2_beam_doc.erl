%% @doc Load the documentation from a beam file.
%% This file has code adapted from code:get_doc/1, as we don't want to load
%% the beam file into the VM when fetching the docs from the beam file.
%% @end

-module(zotonicwww2_beam_doc).

-include_lib("kernel/include/eep48.hrl").
% -include_lib("zotonic_core/include/zotonic.hrl").

-export([
    import_docs/1,

    get_doc/1,
    get_modules_doc/1,
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


%% @doc Import the documentation from all beam and md files into the resources.
-spec import_docs(z:context()) -> ok.
import_docs(Context0) ->
    Context = z_acl:sudo(Context0),
    import_modules(Context),
    import_controllers(Context),
    import_filters(Context),
    import_scomps(Context),
    import_models(Context),
    import_actions(Context),
    import_validators(Context),
    import_releases(Context),
    import_tags(Context),
    import_notifications(Context),
    ok.

import_modules(Context) ->
    Mods = get_modules_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            Mod = app2mod(Name),
            PageName = <<"doc_module_", Mod/binary>>,
            Props = #{
                <<"title">> => Mod,
                <<"body">> => Doc
            },
            insert_or_update(module, PageName, Props, Context)
        end,
        Mods).

import_filters(Context) ->
    Mods = get_filters_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                doc := Body,
                app := _App,
                module := Module
            } = Doc,
            <<"filter_", Filter/binary>> = Name,
            PageName = <<"doc_template_filter_filter_", Filter/binary>>,
            Props = #{
                <<"title">> => Filter,
                <<"body">> => Body
            },
            insert_or_update(template_filter, PageName, Props, Module, Context)
        end,
        Mods).

import_scomps(Context) ->
    Mods = get_scomps_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                app := _App,
                doc := Body,
                module := Module
            } = Doc,
            Scomp = remove_mod(Name, Module),
            PageName = <<"doc_template_scomp_scomp_", Scomp/binary>>,
            Props = #{
                <<"title">> => Scomp,
                <<"body">> => Body
            },
            insert_or_update(template_scomp, PageName, Props, Module, Context)
        end,
        Mods).

import_models(Context) ->
    Mods = get_models_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                app := _App,
                doc := Body,
                module := Module
            } = Doc,
            <<"m_", Model/binary>> = Name,
            PageName = <<"doc_model_model_", Model/binary>>,
            Props = #{
                <<"title">> => Model,
                <<"body">> => Body
            },
            insert_or_update(model, PageName, Props, Module, Context)
        end,
        Mods).

import_validators(Context) ->
    Mods = get_validators_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                app := _App,
                doc := Body,
                module := Module
            } = Doc,
            Validator = remove_mod(Name, Module),
            PageName = <<"doc_template_validator_validator_", Validator/binary>>,
            Props = #{
                <<"title">> => Validator,
                <<"body">> => Body
            },
            insert_or_update(template_validator, PageName, Props, Module, Context)
        end,
        Mods).

import_actions(Context) ->
    Mods = get_actions_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                app := _App,
                doc := Body,
                module := Module
            } = Doc,
            Action = remove_mod(Name, Module),
            PageName = <<"doc_template_action_action_", Action/binary>>,
            Props = #{
                <<"title">> => Action,
                <<"body">> => Body
            },
            insert_or_update(template_action, PageName, Props, Module, Context)
        end,
        Mods).

import_controllers(Context) ->
    Mods = get_controllers_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            #{
                app := _App,
                doc := Body,
                module := Module
            } = Doc,
            <<"controller_", Controller/binary>> = Name,
            PageName = <<"doc_controller_controller_", Controller/binary>>,
            Props = #{
                <<"title">> => Name,
                <<"body">> => Body
            },
            insert_or_update(controller, PageName, Props, Module, Context)
        end,
        Mods).

import_releases(Context) ->
    Mods = get_releases_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            PageName = <<"doc_releasenotes_rel_", Name/binary>>,
            Props = #{
                <<"title">> => Name,
                <<"body">> => Doc
            },
            insert_or_update(releasenotes, PageName, Props, Context)
        end,
        Mods).

import_tags(Context) ->
    Mods = get_tags_doc(Context),
    maps:foreach(
        fun(Name, Doc) ->
            PageName = <<"doc_template_tag_tag_", Name/binary>>,
            Props = #{
                <<"title">> => Name,
                <<"body">> => Doc
            },
            insert_or_update(template_tag, PageName, Props, Context)
        end,
        Mods).

import_notifications(Context) ->
    Mods = get_notifications_doc(Context),
    maps:foreach(
        fun(_Name, Doc) ->
            #{
                notification := Notification,
                sig := Sig,
                name := _FunctionName,
                arity := Arity,
                doc := Body,
                record := Fields
            } = Doc,
            PageName = <<"doc_notification_", Notification/binary>>,
            Props = #{
                <<"title">> => Sig,
                <<"body">> => Body,
                <<"notification">> => #{
                    <<"name">> => Notification,
                    <<"arity">> => Arity,
                    <<"record">> => Fields
                }
            },
            insert_or_update(notification, PageName, Props, Context)
        end,
        Mods).


insert_or_update(Category, Name, Props, Context) ->
    Props1 = Props#{
        <<"name">> => Name,
        <<"is_published">> => true,
        <<"category_id">> => Category
    },
    case m_rsc:rid(Name, Context) of
        undefined ->
            m_rsc:insert(Props1, Context);
        RId ->
            m_rsc:update(RId, Props1, Context)
    end.

insert_or_update(Category, Name, Props, Module, Context) ->
    {ok, RscId} = insert_or_update(Category, Name, Props, Context),
    ModName = <<"doc_module_", Module/binary>>,
    m_edge:replace(RscId, in_module, [ ModName ], Context).


%% @doc List the modules for which we expect documentation.
-spec get_modules_doc(z:context()) -> #{ binary() => binary() }.
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
                        false -> Acc;
                        DocFilename ->
                            Beamfile = filename:join([ GitDir, "_build", "default", "lib", AppName, "ebin", DocFilename ]),
                            case get_doc(Beamfile) of
                                {ok, #docs_v1{
                                    module_doc = #{ <<"en">> := Doc }
                                }} ->
                                    Acc#{
                                        AppName => z_markdown:to_html(Doc)
                                    };
                                _ ->
                                    Acc
                            end
                    end;
                false ->
                    Acc
            end
        end,
        #{},
        Apps).

%% @doc List the filters for which we expect documentation.
-spec get_filters_doc(z:context()) -> #{ binary() => binary() }.
get_filters_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "filters", "filter_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the models for which we expect documentation.
-spec get_models_doc(z:context()) -> #{ binary() => binary() }.
get_models_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "models", "m_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the scomps for which we expect documentation.
-spec get_scomps_doc(z:context()) -> #{ binary() => binary() }.
get_scomps_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "scomps", "scomp_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the actions for which we expect documentation.
-spec get_actions_doc(z:context()) -> #{ binary() => binary() }.
get_actions_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "actions", "action_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the validators for which we expect documentation.
-spec get_validators_doc(z:context()) -> #{ binary() => binary() }.
get_validators_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "validators", "validator_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the controllers for which we expect documentation.
-spec get_controllers_doc(z:context()) -> #{ binary() => binary() }.
get_controllers_doc(Context) ->
    AppDir = m_zotonicwww2_git:apps_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([AppDir, "*", "src", "controllers", "controller_*.erl"]))),
    get_docs(Files, Context).

%% @doc List the releases and their documentation.
-spec get_releases_doc(z:context()) -> #{ binary() => binary() }.
get_releases_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    Files = filelib:wildcard(unicode:characters_to_list(filename:join([GitDir, "doc", "release-notes", "*.md"]))),
    lists:foldl(
        fun(File, Acc) ->
            Version = unicode:characters_to_binary(filename:rootname(filename:basename(File))),
            {ok, Data} = file:read_file(File),
            Acc#{
                Version => z_markdown:to_html(Data)
            }
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
            Acc#{
                Tag => z_markdown:to_html(Data)
            }
        end,
        #{},
        Files).

%% @doc Inspect the observer behaviour beam file and extract the documentation for all notifications.
-spec get_notifications_doc(z:context()) -> #{ binary() => map() }.
get_notifications_doc(Context) ->
    GitDir = m_zotonicwww2_git:git_dir(Context),
    BeamFile = filename:join([ GitDir, "_build", "default", "lib", "zotonic_core", "ebin", "zotonic_observer.beam" ]),
    {ok, #docs_v1{
        docs = Docs
    }} = get_doc(BeamFile),
    lists:foldl(
        fun
            ({{callback, FunctionName, Arity}, _LineCol, [Sig], #{ <<"en">> := Doc }, _}, Acc) ->
                Notification = case atom_to_binary(FunctionName) of
                    <<"observe_", N/binary>> -> N;
                    <<"pid_observe_", N/binary>> -> N
                end,
                Rec = binary_to_atom(Notification),
                Fields = case m_development:lookup_record(Rec) of
                    {Rec, Fs} -> Fs;
                    none -> undefined
                end,
                Acc#{
                    Sig => #{
                        notification => Notification,
                        sig => Sig,
                        name => FunctionName,
                        arity => Arity,
                        doc => z_markdown:to_html(Doc),
                        record => Fields
                    }
                };
            (_, Acc) ->
                Acc
        end,
        #{},
        Docs).


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
                    module_doc = #{ <<"en">> := Doc }
                }} ->
                   Acc#{
                        Name => #{
                            doc => z_markdown:to_html(Doc),
                            app => AppName,
                            module => app2mod(AppName)
                        }
                    };
                _ ->
                    Acc
            end
        end,
        #{},
        Files).


app_name(Filename) ->
    Filename1 = unicode:characters_to_binary(Filename),
    [Dir, _] = binary:split(Filename1, <<"/src/">>),
    filename:basename(Dir).

app2mod(<<"zotonic_mod_", M/binary>>) -> <<"mod_", M/binary>>;
app2mod(App) -> App.

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

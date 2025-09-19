%% @doc This is a one-time conversion of the documentation from the HTML files
%% to the moduledoc attributes of the Erlang files. This file will be removed
%% in the near future, when the moduledoc updates are merged into Zotonic.
%% @end

-module(zotonicwww2_docs2erl).

-export([
    activate/1,
    deactivate/1,
    move/1,

    import_file/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

activate(Context) ->
    lists:foreach(
        fun(M) -> z_module_manager:activate(M, Context) end,
        extra_mods()).

deactivate(Context) ->
    lists:foreach(
        fun(M) -> z_module_manager:deactivate(M, Context) end,
        extra_mods()).

extra_mods() ->
        [
            mod_comment,
            mod_microsoft,
            mod_email_receive,
            mod_ratelimit,
            mod_signup,
            mod_import_csv,
            mod_linkedin,
            mod_tkvstore,
            mod_email_dkim,
            mod_facebook
        ].

move(Context) ->
    % 1. For all reference docs:
    %    filters / models / actions / scomps / modules / validators / controllers
    % 2. Notifications into the observer behaviour module
    % 3. Tags to /doc/tags/
    % 4. Release notes to /doc/release-notes/
    %
    Fs = zotonicwww2_parse_docs:list_files(fill, Context),
    Fs1 = maps:filter(
        fun
            (_Filename, #{ name := <<"doc_template_filter_filter_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_template_scomp_scomp_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_template_action_action_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_template_validator_validator", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_model_model_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_module_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_controller_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_notification_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_template_tag_tag_", _/binary>> }) ->
                true;
            (_Filename, #{ name := <<"doc_releasenotes_rel_", _/binary>> }) ->
                true;
            (_Filename, #{ name := Name }) ->
                ?DEBUG(Name),
                false
        end,
        Fs),
    maps:foreach(fun(Filename, F) -> import_file(Filename, F, Context) end, Fs1).

import_file(Filename, #{ category := _, name := Name }, Context) ->
    {ok, {#{ <<"body">> := Body } = _Props, _TocAcc, Edges}} = zotonicwww2_parse_docs:parse_file(Filename, Name, #{}),
    MD = z_markdown:to_markdown(z_string:trim(Body)),
    MD1 = z_string:trim(MD),
    update_file(Name, MD1, Context);
    % io:format("~s ~p~n~ts~n~n~ts~n~n========================================================================================~n~n", [Filename, Name, MD1, <<>>]);
import_file(_, _, _Context) ->
    ok.

update_file(<<"doc_template_filter_filter_", Filter/binary>> = Name, Doc, Context) ->
    case z_module_indexer:find(filter, binary_to_atom(Filter), Context) of
        {ok, #module_index{
            filepath = Filepath
        }} ->
            [_,AppPath] = binary:split(Filepath, <<"/_build/default/lib/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            SrcPath = filename:join(AppsDir, AppPath),
            zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
        {error, _} = Error ->
            ?DEBUG({Name, Error}),
            Error
    end;
update_file(<<"doc_template_scomp_scomp_", Scomp/binary>> = Name, Doc, Context) ->
    case z_module_indexer:find(scomp, binary_to_atom(Scomp), Context) of
        {ok, #module_index{
            filepath = Filepath
        }} ->
            [_,AppPath] = binary:split(Filepath, <<"/_build/default/lib/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            SrcPath = filename:join(AppsDir, AppPath),
            zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
        {error, _} = Error ->
            ?DEBUG({Name, Error}),
            Error
    end;
update_file(<<"doc_template_validator_validator_", Validator/binary>> = Name, Doc, Context) ->
    case z_module_indexer:find(validator, binary_to_atom(Validator), Context) of
        {ok, #module_index{
            filepath = Filepath
        }} ->
            [_,AppPath] = binary:split(Filepath, <<"/_build/default/lib/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            SrcPath = filename:join(AppsDir, AppPath),
            zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
        {error, _} = Error ->
            ?DEBUG({Name, Error}),
            Error
    end;
update_file(<<"doc_template_action_action_", Action/binary>> = Name, Doc, Context) ->
    case z_module_indexer:find(action, binary_to_atom(Action), Context) of
        {ok, #module_index{
            filepath = Filepath
        }} ->
            [_,AppPath] = binary:split(Filepath, <<"/_build/default/lib/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            SrcPath = filename:join(AppsDir, AppPath),
            zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
        {error, _} = Error ->
            ?DEBUG({Name, Error}),
            Error
    end;
update_file(<<"doc_model_model_", Model/binary>> = Name, Doc, Context) ->
    case z_module_indexer:find(model, binary_to_atom(Model), Context) of
        {ok, #module_index{
            filepath = Filepath
        }} ->
            [_,AppPath] = binary:split(Filepath, <<"/_build/default/lib/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            SrcPath = filename:join(AppsDir, AppPath),
            zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
        {error, _} = Error ->
            ?DEBUG({Name, Error}),
            Error
    end;
update_file(<<"doc_module_", Mod/binary>>, Doc, Context) ->
    AppsDir = m_zotonicwww2_git:apps_dir(Context),
    SrcPath = filename:join([AppsDir, <<"zotonic_", Mod/binary>>, <<"src">>, <<Mod/binary, ".erl">>]),
    zotonicwww2_docs_add:add_moduledoc_block(SrcPath, Doc);
update_file(<<"doc_controller_", Controller/binary>>, Doc, Context) ->
    Controller1 = binary_to_atom(Controller),
    case code:ensure_loaded(Controller1) of
        {module, Mod} ->
            {compile, Attrs} = proplists:lookup(compile, Mod:module_info()),
            {source, SrcPath} = proplists:lookup(source, Attrs),
            [ _, Path ] = binary:split(unicode:characters_to_binary(SrcPath), <<"/apps/">>),
            AppsDir = m_zotonicwww2_git:apps_dir(Context),
            GitPath = filename:join([AppsDir, Path ]),
            zotonicwww2_docs_add:add_moduledoc_block(GitPath, Doc);
        {error, _} = Error ->
            Error
    end;
update_file(<<"doc_notification_", Notification/binary>>, Doc, Context) ->
    FunName = <<"observe_", Notification/binary>>,
    AppsDir = m_zotonicwww2_git:apps_dir(Context),
    GitPath = filename:join([AppsDir, "zotonic_core", "src", "behaviours", "zotonic_observer.erl" ]),
    zotonicwww2_docs_add:add_callback_doc_block(GitPath, FunName, Doc);
update_file(<<"doc_template_tag_tag_", Tag/binary>>, Doc, Context) ->
    TagsDir = filename:join([m_zotonicwww2_git:git_dir(Context), "doc", "template-tags"]),
    FilePath = filename:join([TagsDir, <<Tag/binary, ".md">>]),
    ok = z_filelib:ensure_dir(FilePath),
    file:write_file(FilePath, Doc);
update_file(<<"doc_releasenotes_rel_", Version/binary>>, Doc, Context) ->
    RelsDir = filename:join([m_zotonicwww2_git:git_dir(Context), "doc", "release-notes"]),
    FilePath = filename:join([RelsDir, <<Version/binary, ".md">>]),
    ok = z_filelib:ensure_dir(FilePath),
    file:write_file(FilePath, Doc).



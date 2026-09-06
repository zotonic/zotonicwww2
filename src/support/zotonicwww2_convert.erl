%% @doc One-off, manually invoked conversions for the zotonic.com documentation.
%%
%% Run `plan/1` first and `run/1` from an Erlang shell when the result has been
%% reviewed. Nothing in this module is called from `manage_schema/2`.
%% @end

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

-module(zotonicwww2_convert).

-export([
    plan/1,
    run/1,
    step_4/1,
    step_5/1,
    step_6/1,
    step_7/1,
    step_8/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type group() :: #{
    name := binary(),
    category := atom()
}.


%% @doc Inspect the resources and edges affected by steps 4 through 8.
%% This function does not change any data.
-spec plan(z:context()) -> {ok, map()} | {error, term()}.
plan(Context0) ->
    Context = z_acl:sudo(Context0),
    Groups = notification_groups() ++ other_groups(),
    case legacy_dispatch_plan(Context) of
        {ok, LegacyDispatch} ->
            {ok, #{
                categories => [category_plan(Name, Context) || Name <- reference_categories()],
                grouping_pages => [group_plan(Group, Context) || Group <- Groups],
                legacy_dispatch => LegacyDispatch,
                totals => #{
                    categories => length(reference_categories()),
                    notification_groups => length(notification_groups()),
                    other_groups => length(other_groups()),
                    groups => length(Groups)
                }
            }};
        {error, _} = Error ->
            Error
    end.


%% @doc Run documentation conversion steps 4 through 8 in order.
%% The conversion is idempotent and stops at the first error.
-spec run(z:context()) -> {ok, map()} | {error, map()}.
run(Context0) ->
    Context = z_acl:sudo(Context0),
    Steps = [
        {step_4, fun step_4/1},
        {step_5, fun step_5/1},
        {step_6, fun step_6/1},
        {step_7, fun step_7/1},
        {step_8, fun step_8/1}
    ],
    run_steps(Steps, #{}, Context).


%% @doc Replace the notification category's imported toctree with an
%% introduction. The faceted category template supplies the item list.
-spec step_4(z:context()) -> {ok, map()} | {error, term()}.
step_4(Context0) ->
    update_introductions([notification], z_acl:sudo(Context0)).


%% @doc Mark the twelve old notification grouping resources as deprecated.
%% Step 6 removes them after their redirects have been registered.
-spec step_5(z:context()) -> {ok, map()} | {error, term()}.
step_5(Context0) ->
    deprecate_groups(notification_groups(), z_acl:sudo(Context0)).


%% @doc Replace the dispatch observer with page-path-log entries and replace
%% the notification grouping pages with their category resource.
-spec step_6(z:context()) -> {ok, map()} | {error, term()}.
step_6(Context0) ->
    Context = z_acl:sudo(Context0),
    case install_legacy_dispatch_paths(Context) of
        {ok, LegacyPaths} ->
            case replace_groups(notification_groups(), Context) of
                {ok, Groups} ->
                    {ok, #{legacy_paths => LegacyPaths, grouping_pages => Groups}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.


%% @doc Remove the manually maintained notification index edges.
-spec step_7(z:context()) -> {ok, map()} | {error, term()}.
step_7(Context0) ->
    remove_index_edges([notification], z_acl:sudo(Context0)).


%% @doc Apply the category introduction, grouping-page replacement and
%% index-edge cleanup to filters, actions, tags, models, controllers and modules.
-spec step_8(z:context()) -> {ok, map()} | {error, term()}.
step_8(Context0) ->
    Context = z_acl:sudo(Context0),
    Categories = other_reference_categories(),
    Groups = other_groups(),
    case update_introductions(Categories, Context) of
        {ok, Introductions} ->
            case deprecate_groups(Groups, Context) of
                {ok, Deprecated} ->
                    case replace_groups(Groups, Context) of
                        {ok, Replaced} ->
                            case remove_index_edges(Categories, Context) of
                                {ok, Edges} ->
                                    {ok, #{
                                        introductions => Introductions,
                                        deprecated => Deprecated,
                                        grouping_pages => Replaced,
                                        index_edges => Edges
                                    }};
                                {error, _} = Error -> Error
                            end;
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.


run_steps([], Results, _Context) ->
    {ok, Results};
run_steps([{Name, Fun} | Rest], Results, Context) ->
    case Fun(Context) of
        {ok, Result} ->
            run_steps(Rest, Results#{Name => Result}, Context);
        {error, Reason} ->
            {error, #{
                failed_step => Name,
                reason => Reason,
                completed => Results
            }}
    end.


update_introductions(Categories, Context) ->
    fold_resources(
        fun(Category, Id) ->
            Props = #{<<"body">> => introduction(Category)},
            update_if_changed(Id, Props, [{is_escape_texts, false}], Context)
        end,
        Categories,
        Context).


deprecate_groups(Groups, Context) ->
    case m_rsc:rid(content_group_deprecated_docs, Context) of
        DeprecatedId when is_integer(DeprecatedId) ->
            fold_groups(
                fun(Group, Id) ->
                    case validate_group_resource(Group, Id, Context) of
                        ok ->
                            Props = #{
                                <<"is_published">> => false,
                                <<"seo_noindex">> => true,
                                <<"content_group_id">> => DeprecatedId,
                                <<"doc_status">> => <<"deprecated">>
                            },
                            update_if_changed(Id, Props, [], Context);
                        {error, _} = Error ->
                            Error
                    end
                end,
                Groups,
                Context);
        undefined ->
            {error, {unknown_resource, content_group_deprecated_docs}}
    end.


%% Store every known old path on the replacement category before deleting the
%% grouping resource. The page-path rows survive the delete because they point
%% directly at the category. `m_rsc:delete/3` additionally records the deleted
%% id and its follow-up category in `rsc_gone`, covering `/docs/ID/slug` and
%% `/id/ID` URLs independently of the page-path log.
replace_groups(Groups, Context) ->
    replace_groups(Groups, empty_replace_result(), Context).

replace_groups([], Result, _Context) ->
    {ok, Result};
replace_groups([#{name := Name, category := Category} = Group | Rest], Result, Context) ->
    case {m_rsc:rid(Name, Context), m_rsc:rid(Category, Context)} of
        {undefined, _} ->
            replace_groups(Rest, add_missing(Name, Result), Context);
        {_, undefined} ->
            {error, {unknown_category, Category}};
        {Id, CategoryId} ->
            case validate_group_resource(Group, Id, Context) of
                ok ->
                    Paths = grouping_page_paths(Id, Name, Context),
                    case register_page_paths(CategoryId, Paths, Context) of
                        {ok, PathCount} ->
                            case m_rsc:delete(Id, CategoryId, Context) of
                                ok ->
                                    Result1 = Result#{
                                        deleted := maps:get(deleted, Result) + 1,
                                        registered_paths := maps:get(registered_paths, Result) + PathCount
                                    },
                                    replace_groups(Rest, Result1, Context);
                                {error, Reason} ->
                                    {error, {delete_with_followup, Name, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {register_page_paths, Name, Reason}}
                    end;
                {error, Reason} ->
                    {error, {invalid_group_resource, Name, Reason}}
            end
    end.


validate_group_resource(#{category := ExpectedCategory}, Id, Context) ->
    ReferenceId = m_rsc:rid(reference, Context),
    ActualCategoryId = m_rsc:p_no_acl(Id, category_id, Context),
    case {ReferenceId, ActualCategoryId} of
        {CategoryId, CategoryId} when is_integer(CategoryId) -> ok;
        _ -> {error, {expected_reference_group, ExpectedCategory, ActualCategoryId}}
    end.


remove_index_edges(Categories, Context) ->
    remove_index_edges(Categories, empty_edge_result(), Context).

remove_index_edges([], Result, _Context) ->
    {ok, Result};
remove_index_edges([Category | Rest], Result, Context) ->
    case m_rsc:rid(Category, Context) of
        undefined ->
            remove_index_edges(Rest, add_missing_edge(Category, Result), Context);
        CategoryId ->
            Count = length(m_edge:objects(CategoryId, haspart, Context)),
            case m_edge:replace(CategoryId, haspart, [], Context) of
                ok ->
                    remove_index_edges(
                        Rest,
                        Result#{removed := maps:get(removed, Result) + Count},
                        Context);
                {error, Reason} ->
                    {error, {remove_haspart, Category, Reason}}
            end
    end.


%% The old dispatch observer converted Sphinx paths to resource names. All
%% resources matching those names receive the same paths in rsc_page_path_log,
%% after which m_rsc:page_path_to_id/2 supplies the permanent redirect.
install_legacy_dispatch_paths(Context) ->
    case legacy_resources(Context) of
        {ok, Rows} -> install_legacy_dispatch_paths(Rows, empty_path_result(), Context);
        {error, _} = Error -> Error
    end.

install_legacy_dispatch_paths([], Result, _Context) ->
    {ok, Result};
install_legacy_dispatch_paths(
    [#{<<"id">> := Id, <<"name">> := Name} | Rest],
    Result,
    Context
) ->
    Paths = legacy_dispatch_paths(Name),
    case Paths of
        [] ->
            install_legacy_dispatch_paths(Rest, Result, Context);
        _ ->
            case redirect_target(Id, Name, Context) of
                undefined ->
                    {error, {unknown_redirect_target, Name}};
                TargetId ->
                    case register_page_paths(TargetId, Paths, Context) of
                        {ok, Count} ->
                            Result1 = Result#{
                                resources := maps:get(resources, Result) + 1,
                                registered_paths := maps:get(registered_paths, Result) + Count
                            },
                            install_legacy_dispatch_paths(Rest, Result1, Context);
                        {error, Reason} ->
                            {error, {register_legacy_path, Name, Reason}}
                    end
            end
    end.


legacy_dispatch_plan(Context) ->
    case legacy_resources(Context) of
        {ok, Rows} ->
            Entries = [
                #{
                    id => Id,
                    name => Name,
                    target_id => redirect_target(Id, Name, Context),
                    paths => Paths
                }
                || #{<<"id">> := Id, <<"name">> := Name} <- Rows,
                   Paths <- [legacy_dispatch_paths(Name)],
                   Paths =/= []
            ],
            {ok, #{
                resources => length(Entries),
                paths => lists:sum([length(maps:get(paths, Entry)) || Entry <- Entries]),
                entries => Entries
            }};
        {error, _} = Error -> Error
    end.


legacy_resources(Context) ->
    z_db:qmap(
        "select id, name from rsc "
        "where name is not null "
        "and (left(name, 4) = 'doc_' "
        "     or name in ('page_home', 'module', 'model', 'controller', "
        "                 'template_filter', 'template_action', 'template_validator', "
        "                 'template_scomp', 'template_tag', 'notification')) "
        "order by id",
        Context).


redirect_target(Id, Name, Context) ->
    case group_by_name(Name) of
        #{category := Category} -> m_rsc:rid(Category, Context);
        undefined -> Id
    end.


fold_resources(Fun, Names, Context) ->
    fold_resources(Fun, Names, empty_result(), Context).

fold_resources(_Fun, [], Result, _Context) ->
    {ok, Result};
fold_resources(Fun, [Name | Rest], Result, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            fold_resources(Fun, Rest, add_missing(Name, Result), Context);
        Id ->
            case Fun(Name, Id) of
                {ok, Status} ->
                    fold_resources(Fun, Rest, add_status(Status, Result), Context);
                {error, Reason} ->
                    {error, {resource, Name, Reason}}
            end
    end.


fold_groups(Fun, Groups, Context) ->
    fold_groups(Fun, Groups, empty_result(), Context).

fold_groups(_Fun, [], Result, _Context) ->
    {ok, Result};
fold_groups(Fun, [#{name := Name} = Group | Rest], Result, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            fold_groups(Fun, Rest, add_missing(Name, Result), Context);
        Id ->
            case Fun(Group, Id) of
                {ok, Status} ->
                    fold_groups(Fun, Rest, add_status(Status, Result), Context);
                {error, Reason} ->
                    {error, {resource, Name, Reason}}
            end
    end.


update_if_changed(Id, Props, Options, Context) ->
    case maps:fold(
        fun(Key, Value, IsEqual) ->
            IsEqual andalso m_rsc:p_no_acl(Id, Key, Context) =:= Value
        end,
        true,
        Props)
    of
        true ->
            {ok, unchanged};
        false ->
            case m_rsc:update(Id, Props, Options, Context) of
                {ok, _} -> {ok, updated};
                {error, _} = Error -> Error
            end
    end.


register_page_paths(Id, Paths, Context) ->
    register_page_paths(Id, normalize_log_paths(Paths), 0, Context).

register_page_paths(_Id, [], Count, _Context) ->
    {ok, Count};
register_page_paths(Id, [Path | Rest], Count, Context) ->
    case z_db:q(
        "insert into rsc_page_path_log (id, page_path) "
        "values ($1, $2) "
        "on conflict (page_path) do update set id = excluded.id",
        [Id, Path],
        Context)
    of
        1 -> register_page_paths(Id, Rest, Count + 1, Context);
        {error, _} = Error -> Error
    end.


grouping_page_paths(Id, Name, Context) ->
    XDefault = z_context:set_language('x-default', Context),
    configured_page_paths(m_rsc:p_no_acl(Id, page_path, Context)) ++
        page_path_log_paths(Id, Context) ++
        legacy_dispatch_paths(Name) ++
        [
            documentation_url(Id, true, Context),
            documentation_url(Id, false, Context),
            documentation_url(Id, true, XDefault),
            documentation_url(Id, false, XDefault)
        ].


page_path_log_paths(Id, Context) ->
    case z_db:q1(
        "select array_agg(page_path) from rsc_page_path_log where id = $1",
        [Id],
        Context)
    of
        undefined -> [];
        Paths when is_list(Paths) -> Paths
    end.


documentation_url(Id, IsSlug, Context) ->
    Args = case IsSlug of
        true -> [
            {id, Id},
            {slug, m_rsc:p_no_acl(Id, title_slug, Context)}
        ];
        false ->
            [{id, Id}]
    end,
    z_dispatcher:url_for(documentation, Args, Context).


configured_page_paths(undefined) -> [];
configured_page_paths(<<>>) -> [];
configured_page_paths(#trans{tr = Tr}) -> [Path || {_Lang, Path} <- Tr];
configured_page_paths(Path) -> [Path].


normalize_log_paths(Paths) ->
    lists:usort(lists:filtermap(fun normalize_log_path/1, Paths)).

normalize_log_path(undefined) ->
    false;
normalize_log_path(Path0) ->
    Path = m_rsc_update:normalize_page_path(z_convert:to_binary(Path0)),
    case Path =/= <<>> andalso z_string:len(Path) =< 80 of
        true -> {true, Path};
        false -> false
    end.


category_plan(Name, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            #{name => Name, exists => false};
        Id ->
            #{
                name => Name,
                id => Id,
                exists => true,
                introduction_is_current =>
                    m_rsc:p_no_acl(Id, body, Context) =:= introduction(Name),
                haspart_edges => length(m_edge:objects(Id, haspart, Context)),
                logged_paths => page_path_log_count(Id, Context)
            }
    end.


group_plan(#{name := Name, category := Category}, Context) ->
    CategoryId = m_rsc:rid(Category, Context),
    Base = #{
        name => Name,
        category => Category,
        category_id => CategoryId,
        category_url => resource_page_url(CategoryId, Context)
    },
    case m_rsc:rid(Name, Context) of
        undefined ->
            Base#{exists => false};
        Id ->
            Base#{
                id => Id,
                exists => true,
                is_published => m_rsc:p_no_acl(Id, is_published, Context),
                doc_status => m_rsc:p_no_acl(Id, doc_status, Context),
                paths_to_log => normalize_log_paths(grouping_page_paths(Id, Name, Context))
            }
    end.


resource_page_url(undefined, _Context) ->
    undefined;
resource_page_url(Id, Context) ->
    m_rsc:p_no_acl(Id, page_url, Context).


page_path_log_count(undefined, _Context) -> 0;
page_path_log_count(Id, Context) ->
    z_db:q1("select count(*) from rsc_page_path_log where id = $1", [Id], Context).


empty_result() ->
    #{updated => 0, unchanged => 0, missing => []}.

add_status(updated, Result) ->
    Result#{updated := maps:get(updated, Result) + 1};
add_status(unchanged, Result) ->
    Result#{unchanged := maps:get(unchanged, Result) + 1}.

add_missing(Name, Result) ->
    Result#{missing := [Name | maps:get(missing, Result)]}.

empty_replace_result() ->
    #{deleted => 0, missing => [], registered_paths => 0}.

empty_path_result() ->
    #{resources => 0, registered_paths => 0}.

empty_edge_result() ->
    #{removed => 0, missing => []}.

add_missing_edge(Name, Result) ->
    Result#{missing := [Name | maps:get(missing, Result)]}.


reference_categories() ->
    [notification | other_reference_categories()].

other_reference_categories() ->
    [template_filter, template_action, template_tag, model, controller, module].


introduction(notification) ->
    <<"<p>Notifications are messages sent by Zotonic at defined points in its processing. "
      "Observe them in Erlang modules to extend or modify behavior. Use the facets below "
      "to browse notifications by subject and module.</p>">>;
introduction(template_filter) ->
    <<"<p>Template filters transform values before they are rendered. Use the facets below "
      "to find filters by data type, task, subject, or module.</p>">>;
introduction(template_action) ->
    <<"<p>Template actions define browser-side behavior triggered by wires and postbacks. "
      "Use the facets below to find actions by task, subject, or module.</p>">>;
introduction(template_tag) ->
    <<"<p>Template tags add logic, composition, and reusable functionality to templates. "
      "Use the facets below to browse tags by subject and module.</p>">>;
introduction(model) ->
    <<"<p>Models expose Zotonic data and services to templates and Erlang code. Use the "
      "facets below to find models by subject and module.</p>">>;
introduction(controller) ->
    <<"<p>Controllers handle HTTP requests and produce responses. Use the facets below to "
      "find controllers by subject, task, or module.</p>">>;
introduction(module) ->
    <<"<p>Modules package reusable Zotonic functionality and integration points. Use the "
      "facets below to explore modules by subject and capability.</p>">>.


%% Reverse mapping for the paths handled by the former observe_dispatch/2.
legacy_dispatch_paths(<<"page_home">>) ->
    [<<"/latest/index.html">>];
legacy_dispatch_paths(<<"module">>) -> index_paths(<<"ref/modules">>);
legacy_dispatch_paths(<<"model">>) -> index_paths(<<"ref/models">>);
legacy_dispatch_paths(<<"controller">>) -> index_paths(<<"ref/controllers">>);
legacy_dispatch_paths(<<"template_filter">>) -> index_paths(<<"ref/filters">>);
legacy_dispatch_paths(<<"template_action">>) -> index_paths(<<"ref/actions">>);
legacy_dispatch_paths(<<"template_validator">>) -> index_paths(<<"ref/validators">>);
legacy_dispatch_paths(<<"template_scomp">>) -> index_paths(<<"ref/scomps">>);
legacy_dispatch_paths(<<"template_tag">>) -> index_paths(<<"ref/tags">>);
legacy_dispatch_paths(<<"notification">>) -> index_paths(<<"ref/notifications">>);
legacy_dispatch_paths(<<"doc_filters_", Group/binary>>) ->
    legacy_paths(<<"ref/filters/", Group/binary, "/index.html">>);
legacy_dispatch_paths(<<"doc_actions_", Group/binary>>) ->
    legacy_paths(<<"ref/actions/", Group/binary, "/index.html">>);
legacy_dispatch_paths(<<"doc_template_filter_", Name/binary>>) ->
    doc_paths(<<"ref/filters">>, Name);
legacy_dispatch_paths(<<"doc_template_action_", Name/binary>>) ->
    doc_paths(<<"ref/actions">>, Name);
legacy_dispatch_paths(<<"doc_module_", Name/binary>>) ->
    doc_paths(<<"ref/modules">>, Name);
legacy_dispatch_paths(<<"doc_model_", Name/binary>>) ->
    doc_paths(<<"ref/models">>, Name);
legacy_dispatch_paths(<<"doc_template_scomp_", Name/binary>>) ->
    doc_paths(<<"ref/scomps">>, Name);
legacy_dispatch_paths(<<"doc_template_tag_", Name/binary>>) ->
    doc_paths(<<"ref/tags">>, Name);
legacy_dispatch_paths(<<"doc_template_validator_", Name/binary>>) ->
    doc_paths(<<"ref/validators">>, Name);
legacy_dispatch_paths(<<"doc_controller__acl_options">>) ->
    doc_paths(<<"ref/controllers">>, <<"acl_options">>);
legacy_dispatch_paths(<<"doc_controller_", Name/binary>>) ->
    doc_paths(<<"ref/controllers">>, Name);
legacy_dispatch_paths(<<"doc_dispatch_", Name/binary>>) ->
    doc_paths(<<"ref/dispatch">>, Name);
legacy_dispatch_paths(<<"doc_notification_", Name/binary>>) ->
    doc_paths(<<"ref/notifications/notification">>, Name);
legacy_dispatch_paths(<<"doc_reference_notifications_", Name/binary>>) ->
    doc_paths(<<"ref/notifications">>, Name);
legacy_dispatch_paths(<<"doc_bestpractices_", Name/binary>>) ->
    doc_paths(<<"best-practices">>, Name);
legacy_dispatch_paths(<<"doc_cookbook_", Name/binary>>) ->
    cookbook_paths(Name);
legacy_dispatch_paths(<<"doc_developerguide_deployment_", Name/binary>>) ->
    doc_paths(<<"developer-guide/deployment">>, Name);
legacy_dispatch_paths(<<"doc_releasenotes_", Name/binary>>) ->
    doc_paths(<<"developer-guide/releasenotes">>, Name);
legacy_dispatch_paths(<<"doc_developerguide_configuration_", Name/binary>>) ->
    doc_paths(<<"ref/configuration">>, Name);
legacy_dispatch_paths(<<"doc_reference_installation_", Name/binary>>) ->
    doc_paths(<<"ref/installation">>, Name);
legacy_dispatch_paths(<<"doc_reference_cli_", Name/binary>>) ->
    doc_paths(<<"ref/cli">>, Name);
legacy_dispatch_paths(<<"doc_developerguide_", Name/binary>>) ->
    doc_paths(<<"developer-guide">>, Name) ++ doc_paths(<<"dev">>, Name);
legacy_dispatch_paths(<<"doc_reference_", Name/binary>>) ->
    doc_paths(<<"ref">>, Name);
legacy_dispatch_paths(<<"doc_userguide_", Name/binary>>) ->
    doc_paths(<<"user-guide">>, Name);
legacy_dispatch_paths(<<"doc_glossary">>) ->
    doc_paths(<<"html">>, <<"glossary">>);
legacy_dispatch_paths(_) ->
    [].


index_paths(Directory) ->
    legacy_paths(<<Directory/binary, "/index.html">>).

doc_paths(Directory, Name) ->
    legacy_paths(<<Directory/binary, $/, Name/binary, ".html">>).

legacy_paths(Path) ->
    [
        <<"/latest/", Path/binary>>,
        <<"/docs/latest/", Path/binary>>
    ].

cookbook_paths(Name) ->
    Direct = doc_paths(<<"cookbook">>, Name),
    case binary:split(Name, <<"_">>) of
        [Sub, Rest] -> Direct ++ doc_paths(<<"cookbook/", Sub/binary>>, Rest);
        [_] -> Direct
    end.


notification_groups() ->
    [
        group(<<"doc_reference_notifications_acl">>, notification),
        group(<<"doc_reference_notifications_auth">>, notification),
        group(<<"doc_reference_notifications_dispatch_url">>, notification),
        group(<<"doc_reference_notifications_edge">>, notification),
        group(<<"doc_reference_notifications_email">>, notification),
        group(<<"doc_reference_notifications_import_export">>, notification),
        group(<<"doc_reference_notifications_media">>, notification),
        group(<<"doc_reference_notifications_pivot">>, notification),
        group(<<"doc_reference_notifications_rsc">>, notification),
        group(<<"doc_reference_notifications_survey">>, notification),
        group(<<"doc_reference_notifications_user">>, notification),
        group(<<"doc_reference_notifications_other">>, notification)
    ].


other_groups() ->
    filter_groups() ++ action_groups().

filter_groups() ->
    [
        group(<<"doc_filters_binaries">>, template_filter),
        group(<<"doc_filters_booleans">>, template_filter),
        group(<<"doc_filters_dates">>, template_filter),
        group(<<"doc_filters_encryption">>, template_filter),
        group(<<"doc_filters_escaping">>, template_filter),
        group(<<"doc_filters_forms">>, template_filter),
        group(<<"doc_filters_html">>, template_filter),
        group(<<"doc_filters_lists">>, template_filter),
        group(<<"doc_filters_mailinglist">>, template_filter),
        group(<<"doc_filters_menu">>, template_filter),
        group(<<"doc_filters_miscellaneous">>, template_filter),
        group(<<"doc_filters_numbers">>, template_filter),
        group(<<"doc_filters_regex">>, template_filter),
        group(<<"doc_filters_resource_lists">>, template_filter),
        group(<<"doc_filters_resources">>, template_filter),
        group(<<"doc_filters_strings">>, template_filter),
        group(<<"doc_filters_survey">>, template_filter),
        group(<<"doc_filters_translation">>, template_filter),
        group(<<"doc_filters_tuples">>, template_filter),
        group(<<"doc_filters_urls">>, template_filter),
        group(<<"doc_filters_variables">>, template_filter)
    ].

action_groups() ->
    [
        group(<<"doc_actions_notifications">>, template_action),
        group(<<"doc_actions_user">>, template_action),
        group(<<"doc_actions_resources">>, template_action),
        group(<<"doc_actions_page_handling">>, template_action),
        group(<<"doc_actions_actions">>, template_action),
        group(<<"doc_actions_admin">>, template_action),
        group(<<"doc_actions_admin_modules">>, template_action),
        group(<<"doc_actions_backup">>, template_action),
        group(<<"doc_actions_config">>, template_action),
        group(<<"doc_actions_dialogs">>, template_action),
        group(<<"doc_actions_dom">>, template_action),
        group(<<"doc_actions_editor">>, template_action),
        group(<<"doc_actions_events">>, template_action),
        group(<<"doc_actions_forms">>, template_action),
        group(<<"doc_actions_javascript">>, template_action),
        group(<<"doc_actions_mailinglist">>, template_action),
        group(<<"doc_actions_predicates">>, template_action),
        group(<<"doc_actions_search">>, template_action),
        group(<<"doc_actions_templates">>, template_action)
    ].


group_by_name(Name) ->
    case [Group || #{name := GroupName} = Group <- notification_groups() ++ other_groups(),
                   GroupName =:= Name]
    of
        [Group] -> Group;
        [] -> undefined
    end.


-spec group(binary(), atom()) -> group().
group(Name, Category) ->
    #{name => Name, category => Category}.

%% @doc A site is like a module, except that a site application
%% also contains a priv/zotonic_site.config file, from which
%% the system can see that this Erlang application is a Zotonic
%% site. All exports below are also valid for a Zotonic module.
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

-module(zotonicwww2).
-author("Zotonic Team").

% Module attributes - shown in the /admin/modules interface.
-mod_title("zotonicwww2 site").
-mod_description("The Zotonic web site").

% The priority of the module. Higher is lower priority, default
% is 500. As a site module should overrule any defaults in
% Zotonic modules the priority is set to 10. Lower is reserved
% for special modules that might be added later.
-mod_prio(10).

% The datamodel version, as used by the z_module_manager to call
% the manage_schema function.
-mod_schema(21).

% Modules that should be started before this module
% In this case 'acl' as an edge to 'acl_user_group_managers' is
% added in the manage_schema/2 function.
% 'acl' is provided by the mod_acl_user_groups and other modules
% that implement access control.
-mod_depends([ acl, admin, mod_content_groups, mod_search ]).

% Documentation of the configurations for this site module.
-mod_config([
        #{
            module => site,     % if not set, then 'module' defaults to the Erlang module name.
            key => rebuild_secret,
            default => <<>>,    % default, as assumed in the code if the config
                                % is not set.
            description => "Secret key used to secure rebuild requests via the API. "
                           "This key is automatically set when the site is installed."
        },
        #{
            module => site,
            key => rebuild_enabled,
            default => false,
            description => "Set to true to enable rebuild requests via the API."
        },
        #{
            module => site,
            key => rebuild_hash,
            default => <<>>,
            description => "Set after each rebuild to the Git hash of the zotonic git repository."
        }
    ]).

% Exports - if exports change then the module is restarted after
% compilation.
-export([
    manage_schema/2,
    manage_data/2,
    event/2
    ]).

% This is the main header file, it contains useful definitions and
% also includes record defintions, as used by manage_schema/2.
-include_lib("zotonic_core/include/zotonic.hrl").


%%====================================================================
%% support functions go here
%%====================================================================

% The function manage_schema is called upon installation of the site or module.
% It is also called when the -mod_schema attribute at the top of this erlang
% module is incremented.
% If you make any change here then ensure that the schema version number is
% incremented.
% This function is called within a transaction, so it is safe to add any tables
% or other database modifications. Often calls to model initialization functions
% are added here.
% All #datamodel resources are added after the transaction, to prevent conflicts
% with other initialization code.
-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok | #datamodel{}.
manage_schema(_Version, Context) ->
    ok = zotonicwww2_doc_import:install(Context),
    #datamodel{

        % These are the extra categories for our website.
        % First is the unique name for the category, then the
        % "parent" category it belongs to, and then some properties
        % to be inserted.
        % Visually indented so that the hierarchy is clear.
        categories = [
            {documentation, text, [
                {title, <<"Documentation">>}
            ]},
                {userguide, documentation, [
                    {title, <<"User guide">>}
                ]},
                {developerguide, documentation, [
                    {title, <<"Developer guide">>}
                ]},
                {cookbook, documentation, [
                    {title, <<"Cook book">>}
                ]},
                {reference, documentation, [
                    {title, <<"Reference">>}
                ]},
                    {module, reference, [
                        {title, <<"Module">>},
                        {summary, <<"Zotonic applications that package functionality, templates, models, and integration points.">>}
                    ]},
                    {controller, reference, [
                        {title, <<"Controller">>},
                        {summary, <<"HTTP request handlers that connect dispatch rules to rendered or generated responses.">>}
                    ]},
                    {model, reference, [
                        {title, <<"Model">>},
                        {summary, <<"Data APIs available from Erlang, templates, and MQTT through Zotonic's model interface.">>}
                    ]},
                    {dispatch, reference, [
                        {title, <<"Dispatch rules">>},
                        {summary, <<"URL routing rules exposed by modules, including their paths, arguments, and controllers.">>}
                    ]},
                    {template_tag, reference, [
                        {title, <<"Tag">>},
                        {summary, <<"Template language constructs for control flow, inclusion, inheritance, wiring, and output.">>}
                    ]},
                    {template_filter, reference, [
                        {title, <<"Filter">>},
                        {summary, <<"Template value transformations for text, collections, dates, numbers, URLs, and other data.">>}
                    ]},
                    {template_action, reference, [
                        {title, <<"Action">>},
                        {summary, <<"Browser actions that templates attach to events with Zotonic's wiring system.">>}
                    ]},
                    {template_scomp, reference, [
                        {title, <<"Scomp">>},
                        {summary, <<"Server-rendered template components for reusable dynamic page elements.">>}
                    ]},
                    {template_validator, reference, [
                        {title, <<"Input validator">>},
                        {summary, <<"Client- and server-side input validation attached to form fields.">>}
                    ]},
                    {template, reference, [
                        {title, <<"Template">>},
                        {summary, <<"Built-in templates that define pages, emails, and reusable interface fragments.">>}
                    ]},
                    {notification, reference, [
                        {title, <<"Notification">>},
                        {summary, <<"Messages exchanged between Zotonic components to observe events, request data, or alter behavior.">>}
                    ]},
                {releasenotes, documentation, [
                    {title, <<"Release notes">>}
                ]},

                % Examples of sites made with Zotonic
                {website_example, website, [
                    {title, <<"Made with Zotonic">>}
                ]},

                % This is a special sub-category for automated actors.
                % It is used for the Git user (see resources below)
                {robot, person, [
                    {title, <<"Automata">>}
                ]}
        ] ++ zotonicwww2_subject_import:datamodel_categories(),

        % These are resources installed by this module. They can use
        % the categories defined above.
        %
        % In the admin the resources are called 'pages', as that is a
        % concept that is easier to understand for editors.
        resources = [
            % This is the resource (page) for the home page.
            % The page_path is set to "/"" and there is a matching
            % dispatch rule in priv/dispatch/dispatch that matches
            % the "/" path to this resource.
            % The name "page_home" is a convention for the home page.
            % Names for generic pages (about, search etc) are encouraged
            % to start with "page_" to prevent name clashes with categories
            % and predicates (which don't have a prefix for their name).
            {page_home, collection, [
                {title, <<"Zotonic">>},
                {summary, <<"Intro blurb for on home page.">>},
                {body, <<"Longer story displayed on home page.">>},
                {page_path, <<"/">>}
            ]},

            % The core module does not have an application-level module resource,
            % so it is seeded here for grouping imported reference documentation.
            {doc_core, module, [
                {title, <<"Zotonic Core">>},
                {summary, <<"The Zotonic core system implements the basic parts needed for every site.">>}
            ]},

            % This is the user for importing all reference documentation from Git
            {gitbot, robot, [
                {title, <<"Git">>},
                {summary, <<"User for automatic update of the reference documentation.">>}
            ]},

            {content_group_imported_docs, content_group, [
                {title, <<"Imported documentation">>},
                {summary, <<"Documentation synchronized from the Zotonic source repository.">>}
            ]},
            {content_group_deprecated_docs, content_group, [
                {title, <<"Deprecated imported documentation">>},
                {summary, <<"Source documentation which is no longer present in the current Zotonic version.">>}
            ]}
        ],

        % Predicates are the 'labels' on the edges (aka connections)
        % between pages. They give meaning to an edge.
        % Edges are added below.
        % Predicates themselves are just like resources, except that
        % they have an extra list to define the valid subject (from)
        % and object (to) categories.
        predicates = [
            % Edges from documentation to other documentation that is
            % linked from the HTML content on the page.
            % Kept for editorial links and future link extraction from imported
            % Markdown documentation.
            {references,
                [
                    % Resource properties, just like with resources
                    {title, #trans{ tr = [{en, <<"References">>}]}}
                ],
                [
                    % Valid from text resources, to text or media
                    {text, text},
                    {text, media}
                ]
            },

            % Hierarchy between imported subject keywords. The predicate has a
            % private name, so the importer can replace its edges without
            % touching editorially managed relations between keyword pages.
            {subject_topic_broader,
                <<"http://www.w3.org/2004/02/skos/core#broader">>,
                #{
                    <<"title">> => #trans{ tr = [{en, <<"Broader concept">>}]},
                    <<"summary">> => #trans{ tr = [{en,
                        <<"Connects a subject keyword to a broader subject concept.">>
                    }]}
                },
                [
                    {keyword, keyword}
                ]
            },

            % All imported reference documentation receives an edge
            % to the module where the part is defined.
            %
            % This edge is synchronized by zotonicwww2_doc_import.
            {in_module,
                [
                    {title, #trans{ tr = [{en, <<"In module">>}]}}
                ],
                [
                    {documentation, module}
                ]
            },

            % Observer callbacks exported by an imported module create an edge
            % to the corresponding notification documentation page.
            %
            % This edge is synchronized by zotonicwww2_doc_import.
            {observes,
                [
                    {title, #trans{ tr = [{en, <<"Observes">>}]}},
                    {summary, #trans{ tr = [{en,
                        <<"Connects a Zotonic module to a notification it observes.">>
                    }]}}
                ],
                [
                    {module, notification}
                ]
            }
        ],

        % Edges are tuples {subject, predicate, object}
        % The edges are directed from subject to object, with the
        % predicate as the label.
        % In the admin the edges are called 'connections', as that is a
        % concept that is easier to understand for editors.
        edges = [
            % We make the user 'user_git' member of the managers user group.
            % This allows the Git user to perform all updates to the content.
            % The hasusergroup predicate and the acl_user_group_managers are
            % added by the mod_acl_user_groups.
            {gitbot, hasusergroup, acl_user_group_managers}
        ]
    }.


%% This function runs after the schema is installed or updated. It ensures the
%% documentation task configuration and rebuilds the public search facets.
-spec manage_data( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_data(_Version, Context) ->
    case m_config:get_value(site, rebuild_secret, Context) of
        undefined ->
            m_config:set_value(site, rebuild_secret, z_ids:id(), Context);
        <<>> ->
            m_config:set_value(site, rebuild_secret, z_ids:id(), Context);
        _ ->
            ok
    end,
    ok = m_config:set_default_value(zotonicwww2, import_status, <<"idle">>, Context),
    ok = m_config:set_default_value(zotonicwww2, import_stage, <<"idle">>, Context),
    ok = search_facet:ensure_table(Context),
    ok = search_facet:pivot_all(Context),
    ok.


%% @doc Handle signed admin dashboard actions. Every action is authorized again
%% server-side even though Zotonic postbacks are signed.
event(#postback{message=docs_fetch}, Context) ->
    queue_admin_action(fetch, Context);
event(#postback{message=docs_rebuild}, Context) ->
    queue_admin_action(rebuild, Context);
event(#postback{message=docs_update}, Context) ->
    queue_admin_action(update, Context);
event(#postback{message=docs_import}, Context) ->
    queue_admin_action(import, Context);
event(#postback{message=docs_import_keywords}, Context) ->
    import_subject_keywords(Context);
event(#postback{message=docs_migrate_legacy}, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case zotonicwww2_doc_import:migrate_legacy(Context) of
                {ok, #{deprecated := Count}} ->
                    refresh_docs_dashboard(
                        z_render:growl(
                            iolist_to_binary(io_lib:format("Migrated ~p legacy documentation pages.", [Count])),
                            Context));
                {error, import_required} ->
                    refresh_docs_dashboard(
                        z_render:growl_error(
                            ?__("Run a successful documentation import before migrating legacy pages.", Context),
                            Context));
                {error, Reason} ->
                    refresh_docs_dashboard(
                        z_render:growl_error(
                            iolist_to_binary(io_lib:format("Legacy migration failed: ~p", [Reason])),
                            Context))
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to manage documentation imports.", Context), Context)
    end;
event(_Event, Context) ->
    Context.


-spec import_subject_keywords(z:context()) -> z:context().
import_subject_keywords(Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case zotonicwww2_subject_import:import(Context) of
                {ok, _Report} ->
                    z_render:growl(?__("All subject keywords imported.", Context), Context);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonicwww2,
                        text => <<"Could not import subject keywords">>,
                        result => error,
                        reason => Reason
                    }),
                    z_render:growl_error(
                        ?__("Could not import subject keywords. Check the server log for details.", Context),
                        Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to import subject keywords.", Context), Context)
    end.

queue_admin_action(Action, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case m_zotonicwww2_git:queue(Action, Context) of
                {ok, _TaskId} ->
                    refresh_docs_dashboard(
                        z_render:growl(?__("The documentation task has been queued.", Context), Context));
                {error, Reason} ->
                    refresh_docs_dashboard(
                        z_render:growl_error(
                            iolist_to_binary(io_lib:format("Could not queue documentation task: ~p", [Reason])),
                            Context))
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to manage documentation imports.", Context), Context)
    end.

%% @doc Replace every dynamic value and action in the documentation dashboard.
-spec refresh_docs_dashboard(z:context()) -> z:context().
refresh_docs_dashboard(Context) ->
    z_render:update(
        "zotonic-docs-dashboard",
        #render{template = "_admin_dashboard_zotonic_docs_status.tpl"},
        Context).

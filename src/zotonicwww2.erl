%% @doc A site is like a module, except that a site application
%% also contains a priv/zotonic_site.config file, from which
%% the system can see that this Erlang application is a Zotonic
%% site. All exports below are also valid for a Zotonic module.
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2022 Marc Worrell

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
-mod_schema(13).

% Modules that should be started before this module
% In this case 'acl' as an edge to 'acl_user_group_managers' is
% added in the manage_schema/2 function.
% 'acl' is provided by the mod_acl_user_groups and other modules
% that implement access control.
-mod_depends([ acl ]).

% Exports - if exports change then the module is restarted after
% compilation.
-export([
    observe_dispatch/2,
    manage_schema/2,
    manage_data/2
    ]).

% This is the main header file, it contains useful definitions and
% also includes record defintions, as used by manage_schema/2.
-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Check if there is a controller or template matching the path. This observer is
%% called if the dispatcher could not match the request path with the dispatch rules.
%% If this observer returns 'undefined' then the next observer is called.
%% Check the list of observers in /admin/development/observers
observe_dispatch(#dispatch{ path = Path }, Context) ->
    % Split the path on "/"
    case lists:reverse(binary:split(Path, <<"/">>, [ global, trim_all ])) of
        [ File | Dirs ] ->
            % Remove the ".html" extension
            Rootname = filename:rootname(File),
            % Let the git doc import routines map the filenam and path
            % to a resource name.
            case zotonicwww2_parse_docs:filename_to_name(Rootname, Dirs) of
                {Name, _Cat, _} ->
                    % Check if the resource name exists.
                    case m_rsc:rid(Name, Context) of
                        undefined ->
                            undefined;
                        RscId ->
                            % Map the path to the given resource id
                            {ok, RscId}
                    end;
                error ->
                    undefined
            end;
        _ ->
            undefined
    end.


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
manage_schema(_Version, _Context) ->
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
                        {title, <<"Module">>}
                    ]},
                    {controller, reference, [
                        {title, <<"Controller">>}
                    ]},
                    {model, reference, [
                        {title, <<"Model">>}
                    ]},
                    {dispatch, reference, [
                        {title, <<"Dispatch rules">>}
                    ]},
                    {template_tag, reference, [
                        {title, <<"Tag">>}
                    ]},
                    {template_filter, reference, [
                        {title, <<"Filter">>}
                    ]},
                    {template_action, reference, [
                        {title, <<"Action">>}
                    ]},
                    {template_scomp, reference, [
                        {title, <<"Scomp">>}
                    ]},
                    {template_validator, reference, [
                        {title, <<"Input validator">>}
                    ]},
                    {template, reference, [
                        {title, <<"Template">>}
                    ]},
                    {notification, reference, [
                        {title, <<"Notificaion">>}
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
        ],

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

            % The "zotonic core" module, it is not in the rst documentation, so it is
            % added here. Filters and other reference documentation can refer to this
            % module.
            {doc_core, module, [
                {title, <<"Zotonic Core">>},
                {summary, <<"The Zotonic core system implements the basic parts needed for every site.">>}
            ]},

            % This is the user for importing all reference documentation from Git
            {gitbot, robot, [
                {title, <<"Git">>},
                {summary, <<"User for automatic update of the reference documentation.">>}
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
            % These links are extracted by the zotonicwww_parse_docs
            % erlang module.
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

            % All imported reference documentation receives an edge
            % to the module where the part is defined.
            %
            % This edge is extracted by the zotonicwww_parse_docs
            % erlang module.
            {in_module,
                [
                    {title, #trans{ tr = [{en, <<"In module">>}]}}
                ],
                [
                    {documentation, module}
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


%% This function runs after the schema is installed or updated. In this case
%% it is ensured that the 'rebuild_secret' configurarion key is set.
%% This key is used in 'src/models/m_zotonicwww2_git.erl'
-spec manage_data( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_data(_Version, Context) ->
    case m_config:get_value(site, rebuild_secret, Context) of
        undefined ->
            m_config:set_value(site, rebuild_secret, z_ids:id(), Context);
        <<>> ->
            m_config:set_value(site, rebuild_secret, z_ids:id(), Context);
        _ ->
            ok
    end.

%% @doc Routines to support mapping of old URLs to the new resource
%% based docs.
%% @end

-module(zotonicwww2_docs).

-export([
    filename_to_name/2
    ]).

% Map a filename and path to its unique name and category.
% Also denote if the file should always be imported (true) or
% only with the initial full import (false).
filename_to_name(<<"index">>, [ Group, <<"filters">> | _ ]) ->
    {<<"doc_filters_", Group/binary>>, reference, true};
filename_to_name(<<"index">>, [ Group, <<"actions">> | _ ]) ->
    {<<"doc_actions_", Group/binary>>, reference, true};
%
% Use the categories for some index files.
filename_to_name(<<"index">>, [ <<"modules">> | _ ]) ->
    {<<"module">>, category, true};
filename_to_name(<<"index">>, [ <<"models">> | _ ]) ->
    {<<"model">>, category, true};
filename_to_name(<<"index">>, [ <<"controllers">> | _ ]) ->
    {<<"controller">>, category, true};
filename_to_name(<<"index">>, [ <<"filters">> | _ ]) ->
    {<<"template_filter">>, category, true};
filename_to_name(<<"index">>, [ <<"actions">> | _ ]) ->
    {<<"template_action">>, category, true};
filename_to_name(<<"index">>, [ <<"validators">> | _ ]) ->
    {<<"template_validator">>, category, true};
filename_to_name(<<"index">>, [ <<"scomps">> | _ ]) ->
    {<<"template_scomp">>, category, true};
filename_to_name(<<"index">>, [ <<"tags">> | _ ]) ->
    {<<"template_tag">>, category, true};
filename_to_name(<<"index">>, [ <<"notifications">>, <<"ref">> | _ ]) ->
    {<<"notification">>, category, true};
%
filename_to_name(<<"filter_", _/binary>> = Filter, [ <<"filters">> | _ ]) ->
    {<<"doc_template_filter_", Filter/binary>>, template_filter, true};
filename_to_name(<<"action_", _/binary>> = Filter, [ <<"actions">> | _ ]) ->
    {<<"doc_template_action_", Filter/binary>>, template_action, true};
filename_to_name(<<"mod_", _/binary>> = Name, [ <<"modules">> | _ ]) ->
    {<<"doc_module_", Name/binary>>, module, true};
filename_to_name(<<"model_", _/binary>> = Name, [ <<"models">> | _ ]) ->
    {<<"doc_model_", Name/binary>>, model, true};
filename_to_name(<<"scomp_", _/binary>> = Name, [ <<"scomps">> | _ ]) ->
    {<<"doc_template_scomp_", Name/binary>>, template_scomp, true};
filename_to_name(<<"tag_", _/binary>> = Name, [ <<"tags">> | _ ]) ->
    {<<"doc_template_tag_", Name/binary>>, template_tag, true};
filename_to_name(<<"validator_", _/binary>> = Name, [ <<"validators">> | _ ]) ->
    {<<"doc_template_validator_", Name/binary>>, template_validator, true};
filename_to_name(<<"controller_", _/binary>> = Name, [ <<"controllers">> | _ ]) ->
    {<<"doc_controller_", Name/binary>>, controller, true};
filename_to_name(Name, [ <<"dispatch">>, <<"ref">> | _ ]) ->
    {<<"doc_dispatch_", Name/binary>>, dispatch, true};
filename_to_name(Name, [ <<"notification">>, <<"notifications">>, <<"ref">> | _ ]) ->
    {<<"doc_notification_", Name/binary>>, notification, true};
%
filename_to_name(<<"acl_options">>, [ <<"controllers">> | _ ]) ->
    {<<"doc_controller__acl_options">>, reference, true};
%
filename_to_name(Name, [ <<"notifications">>, <<"ref">> | _ ]) ->
    {<<"doc_reference_notifications_", Name/binary>>, reference, true};
%
filename_to_name(Name, [ <<"best-practices">> | _ ]) ->
    {<<"doc_bestpractices_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ Sub, <<"cookbook">> | _ ]) ->
    {<<"doc_cookbook_", Sub/binary, "_", Name/binary>>, cookbook, false};
filename_to_name(Name, [ <<"cookbook">> | _ ]) ->
    {<<"doc_cookbook_", Name/binary>>, cookbook, false};
filename_to_name(Name, [ <<"developer-guide">> | _ ]) ->
    {<<"doc_developerguide_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"dev">> | _ ]) ->
    {<<"doc_developerguide_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"ref">> | _ ]) ->
    {<<"doc_reference_", Name/binary>>, reference, true};
filename_to_name(Name, [ <<"user-guide">> | _ ]) ->
    {<<"doc_userguide_", Name/binary>>, userguide, false};
%
filename_to_name(Name, [ <<"deployment">>, <<"developer-guide">> | _ ]) ->
    {<<"doc_developerguide_deployment_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"releasenotes">>, <<"developer-guide">> | _ ]) ->
    {<<"doc_releasenotes_", Name/binary>>, releasenotes, true};
%
filename_to_name(Name, [ <<"configuration">>, <<"ref">> | _ ]) ->
    {<<"doc_developerguide_configuration_", Name/binary>>, reference, true};
filename_to_name(Name, [ <<"installation">>, <<"ref">> | _ ]) ->
    {<<"doc_reference_installation_", Name/binary>>, reference, true};
filename_to_name(Name, [ <<"cli">>, <<"ref">> | _ ]) ->
    {<<"doc_reference_cli_", Name/binary>>, reference, true};
%
filename_to_name(<<"glossary">>, [ <<"html">> | _ ]) ->
    {<<"doc_glossary">>, documentation, true};
%
% Templates should be done by being able to show the template from
% the git checkout.
filename_to_name(_Name, [ <<"templates">> | _ ]) ->
    error;
%
filename_to_name(Name, RevPath) ->
    io:format("No link map for: ~p in ~p~n", [ Name, RevPath ]),
    error.

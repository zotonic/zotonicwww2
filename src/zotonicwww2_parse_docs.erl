%% @doc The technical documentation is written in ReStructured Text, and stored
%% in the Git repository. This assumes the docs are generated using sphinx and
%% then synchronizes the found HTML with resources in the database.
%%
%% This makes it possible to better search and relate the documentation.
%%
%% If some documentation is not found then the mirrored version is unpublished.
%% Documentation is never deleted to prevent "dangling" references or removal
%% of valuable data on temporary errors.
%%

-module(zotonicwww2_parse_docs).

-export([
    import_one_time_only/1,
    import_all/1,
    list_files/2,
    parse_file/1,

    cleanup_do_not_run/1,
    cleanup_edges_do_not_run/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

import_one_time_only(Context) ->
    do_import(true, Context).

import_all(Context) ->
    do_import(false, Context).

do_import(IsFull, Context0) when is_boolean(IsFull) ->
    % Use the special 'gitbot' user to perform all imports.
    Context = z_acl:logon(gitbot, Context0),
    % Fetch all generated html files to be imported
    Fs = list_files(IsFull, Context),
    % Import each found file as a resource into the database
    maps:fold(
        fun(F, Type, Stat) ->
            #{
                category := Cat,
                name := Name
            } = Type,
            {ok, {Props, Edges}} = parse_file(F),
            Props1 = Props#{
                <<"is_published">> => true,
                <<"category">> => Cat,
                <<"name">> => Name,
                <<"github_url">> => github_url(F)
            },
            lager:info("Updating: ~s", [ Name ]),
            {ok, RscId} = case m_rsc:rid(Name, Context) of
                undefined ->
                    m_rsc:insert(Props1, Context);
                ExistingId ->
                    m_rsc:update(ExistingId, Props1, Context)
            end,
            add_edges(RscId, Edges, Context),
            Count = maps:get(Cat, Stat, 0),
            Stat#{ Cat => Count + 1 }
        end,
        #{},
        Fs).

github_url(F) ->
    F1 = re:replace(F, ".html$", ".rst"),
    Fs = filename:split( iolist_to_binary(F1) ),
    [ <<"doc">>, <<"_build">>, <<"html">> | Fs1 ] = lists:dropwhile( fun(P) -> P =/= <<"doc">> end, Fs ),
    iolist_to_binary([
            <<"https://github.com/zotonic/zotonic/tree/master/doc">>,
            [ [ $/, Part ] || Part <- Fs1 ]
        ]).

add_edges(SubjectId, Edges, Context) ->
    lists:foreach(
        fun({Predicate, ObjectName}) when is_atom(Predicate) ->
            ObjectId = case m_rsc:rid(ObjectName, Context) of
                undefined ->
                    Dummy = #{
                        <<"category">> => <<"other">>,
                        <<"name">> => ObjectName,
                        <<"title">> => <<"Placeholder: ", ObjectName/binary>>,
                        <<"is_published">> => false
                    },
                    {ok, ObjId} = m_rsc:insert(Dummy, Context),
                    ObjId;
                ObjId ->
                    ObjId
            end,

            case m_rsc:p(ObjectId, category_id, Context) of
                SubjectId when Predicate =:= references ->
                    % Ignore references from a category (index) to pages
                    % of that same category
                    ok;
                _ ->
                    m_edge:insert(SubjectId, Predicate, ObjectId, Context)
            end
        end,
        Edges).

%% @doc List all files in the generated doc directory. Returns a map
%% with per filename the category and unique name.
-spec list_files( boolean(), z:context() ) -> map().
list_files(IsFull, Context) ->
    DocDir = unicode:characters_to_list( m_zotonicwww2_git:doc_dir(Context) ),
    filelib:fold_files(
        DocDir,
        "\\.html$",
        true,
        fun(F, Acc) ->
            list_fun(IsFull, F, Acc)
        end,
        #{}).

list_fun(IsFull, F, Acc) ->
    F1 = unicode:characters_to_binary(F),
    [ Basename | RevPath ] = lists:reverse( filename:split(F1) ),
    Rootname = filename:rootname(Basename),
    Rootname1 = binary:replace(Rootname, <<"-">>, <<"_">>, [ global ]),
    case filename_to_name(Rootname1, RevPath) of
        {_Name, undefined, _IsAlways} ->
            error;
        {Name, Cat, IsAlways} when IsAlways orelse IsFull ->
            C = #{
                category => Cat,
                name => Name
            },
            Acc#{
                F => C
            };
        _ ->
            Acc
    end.

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
%
filename_to_name(<<"acl_options">>, [ <<"controllers">> | _ ]) ->
    {<<"doc_controller__acl_options">>, controller, true};
%
filename_to_name(Name, [ <<"notifications">>, <<"ref">> | _ ]) ->
    {<<"doc_reference_notifications_", Name/binary>>, reference, true};
%
filename_to_name(Name, [ <<"best-practices">> | _ ]) ->
    {<<"doc_bestpractices_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"cookbook">> | _ ]) ->
    {<<"doc_cookbook_", Name/binary>>, cookbook, false};
filename_to_name(Name, [ <<"developer-guide">> | _ ]) ->
    {<<"doc_developerguide_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"ref">> | _ ]) ->
    {<<"doc_reference_", Name/binary>>, reference, true};
filename_to_name(Name, [ <<"user-guide">> | _ ]) ->
    {<<"doc_userguide_", Name/binary>>, userguide, false};
%
filename_to_name(Name, [ <<"deployment">>, <<"developer-guide">> | _ ]) ->
    {<<"doc_developerguide_deployment_", Name/binary>>, developerguide, false};
filename_to_name(Name, [ <<"releasenotes">>, <<"developer-guide">> | _ ]) ->
    {<<"doc_releasenotes_", Name/binary>>, releasenotes, false};
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


parse_file(F) ->
    {ok, Bin} = file:read_file(F),
    {ok, Html} = z_html_parse:parse(Bin),
    find_main(F, Html).


find_main(File, Html) ->
    FMain = fun(Args) ->
        proplists:get_value(<<"role">>, Args) =:= <<"main">>
    end,
    {ok, Main} = find_element(<<"div">>, FMain, Html),
    {ok, Title} = find_element(<<"h1">>, fun(_) -> true end, Main),
    [ _Filename | RevPath ] = lists:reverse( filename:split(File) ),
    #{
        in_module := InModule,
        links := Links,
        seealso := Seealso,
        toctree := TocTree
    } = extract_props(Main, RevPath),
    References = Links -- TocTree,
    Edges = map_edge(References, references)
         ++ map_edge(Seealso, relation)
         ++ map_edge(InModule, in_module)
         ++ map_edge(TocTree, haspart),
    {ok, {
        #{
            <<"title">> => text(Title),
            <<"body">> => z_string:trim(flatten(Main, RevPath))
        },
        Edges}
    }.

map_edge(Links, Predicate) ->
    lists:foldl(
        fun
            (<<"/id/", Name/binary>>, Acc) ->
                [ {Predicate, Name} | Acc ];
            (_, Acc) ->
                Acc
        end,
        [],
        Links).

text(Html) ->
    z_string:trim(text(Html, <<>>)).

text(<<"¶"/utf8>>, Acc) ->
    Acc;
text(Bin, Acc) when is_binary(Bin) ->
    <<Acc/binary, Bin/binary>>;
text(L, Acc) when is_list(L) ->
    lists:foldl(
        fun(A, As) ->
            text(A, As)
        end,
        Acc,
        L);
text({_, _, Elts}, Acc) ->
    text(Elts, Acc);
text(_, Acc) ->
    Acc.

find_element(Elt, F, {Elt, Args, Elts}) ->
    case F(Args) of
        true ->
            {ok, Elts};
        false ->
            find_element(Elt, F, Elts)
    end;
find_element(Elt, F, {_, _, Elts}) ->
    find_element(Elt, F, Elts);
find_element(Elt, F, [ E | Es ]) ->
    case find_element(Elt, F, E) of
        error -> find_element(Elt, F, Es);
        Other -> Other
    end;
find_element(_Elt, _F, _) ->
    error.

%% @doc Flatten the sanitized html tree to a binary
-spec flatten( z_html_parse:html_element(), [ binary() ] ) -> binary().
flatten(B, _RevPath) when is_binary(B) ->
    z_html:escape(B);
flatten({nop, Enclosed}, RevPath) ->
    flatten(Enclosed, RevPath);
flatten({comment, _Text}, _RevPath) ->
    <<>>;
flatten({<<"colgroup">>, _, _}, _RevPath) ->
    <<>>;
flatten({<<"blockquote">>, _Attrs, Enclosed}, RevPath) ->
    % Sphinx surrounds some normal ul-lists with the
    % blockquote tag. This make for strange markup and there
    % are no real block quotes in the reference docs, so just
    % remove all blockquotes from the imported HTML.
    flatten(Enclosed, RevPath);
flatten({<<"table">>, _, _} = Elt, RevPath) ->
    [
        <<"<div class='table-wrapper'>">>,
        flatten_elt(Elt, RevPath),
        <<"</div>">>
    ];
flatten({_, _, _} = Elt, RevPath) ->
    flatten_elt(Elt, RevPath);
flatten(L, RevPath) when is_list(L) ->
    iolist_to_binary([ flatten(A, RevPath) || A <- L ]).


flatten_elt({Elt, Attrs, Enclosed}, RevPath) ->
    case is_ignore(Elt, Attrs, Enclosed) of
        true ->
            <<>>;
        false ->
            EncBin = flatten(Enclosed, RevPath),
            Attrs1 = [flatten_attr(Attr, RevPath) || Attr <- Attrs ],
            Attrs2 = iolist_to_binary(prefix(32, Attrs1)),
            case is_selfclosing(Elt) andalso EncBin == <<>> of
                true ->  <<$<, Elt/binary, Attrs2/binary, 32, $/, $>>>;
                false -> <<$<, Elt/binary, Attrs2/binary, $>, EncBin/binary, $<, $/, Elt/binary, $>>>
            end
    end.

is_ignore(<<"h1">>, _, _Enclosed) ->
    % There is only a single h1 title, it is copied to the title
    % property, so it can be removed from the imported HTML.
    true;
is_ignore(<<"a">>, Attrs, _Enclosed) ->
    % No generated links, only organic links from the written
    % documentation.
    case proplists:get_value(<<"class">>, Attrs) of
        <<"headerlink">> -> true;
        _ -> false
    end;
is_ignore(_, Attrs, Enclosed) ->
    Class = proplists:get_value(<<"class">>, Attrs, <<>>),
    Classes = binary:split(Class, <<" ">>, [ global ]),
    lists:any(
        fun(C) ->
            is_ignore_class(C, Enclosed)
        end,
        Classes).

% % Elements that are removed from the html in the body
% is_ignore_class(<<"admonition-see-also">>, _Enclosed) ->
%     % The see-also are now edges, remove the HTML
%     true;
% is_ignore_class(<<"seealso">>, _Enclosed) ->
%     % The see-also are now edges, remove the HTML
%     true;
% is_ignore_class(<<"admonition">>, _Enclosed) ->
%     true;
is_ignore_class(<<"simple">>, Enclosed) ->
    % There is a special link referring to the module. This is
    % not an edge, so the HTML is removed.
    case text(Enclosed) of
        <<"Module: ", _/binary>> -> true;
        _ -> false
    end;
is_ignore_class(_, _) ->
    false.

prefix(Sep, List) -> prefix(Sep,List,[]).
prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).

%% @doc Flatten an attribute to a binary, filter urls and css.
flatten_attr({<<"style">>,_Value}, _RevPath) ->
    <<>>;
flatten_attr({<<"item", _/binary>>,_Value}, _RevPath) ->
    <<>>;
flatten_attr({<<"class">>,Value}, _RevPath) ->
    case filter_class(Value) of
        <<>> -> <<>>;
        V ->
            Value1 = z_html:escape(V),
            <<"class=\"", Value1/binary, $">>
    end;
flatten_attr({Attr,Value}, RevPath) ->
    Value1 = case is_url_attr(Attr) of
                true -> fix_link(Attr, Value, RevPath);
                false -> Value
            end,
    Value2 = z_html:escape(Value1),
    <<Attr/binary, $=, $", Value2/binary, $">>.

%% Change internal links to "/id/name"
fix_link(_Attr, <<>>, _RevPath) ->
    <<>>;
fix_link(_Attr, <<"http:", _/binary>> = Link, _RevPath) ->
    Link;
fix_link(_Attr, <<"https:", _/binary>> = Link, _RevPath) ->
    Link;
fix_link(_Attr, <<"mailto:", _/binary>> = Link, _RevPath) ->
    Link;
fix_link(_Attr, <<"#", _/binary>> = Link, _RevPath) ->
    Link;
fix_link(<<"href">>, Href, RevPath) ->
    case binary:split(Href, <<"#">>) of
        [ Path | Hash ] ->
            {Basename, RevPathBase} = relpath(Path, RevPath),
            Root = filename:rootname(Basename),
            case map_link(Root, RevPathBase) of
                error -> Href;
                NewHref -> add_hash(NewHref, Hash)
            end;
        _ ->
            Href
    end;
fix_link(_, Link, _RevPath) ->
    Link.

add_hash(Href, []) ->
    Href;
add_hash(Href, [ Hash ]) ->
    <<Href/binary, "#", Hash/binary>>.


relpath(Link, RevPath) ->
    LinkPs = binary:split(Link, <<"/">>, [global]),
    relpath_1(LinkPs, RevPath).

relpath_1([], RevPath) ->
    {<<"index">>, RevPath};
relpath_1([ <<".">> | Ps ], RevPath) ->
    relpath_1(Ps, RevPath);
relpath_1([ <<"..">> | Ps ], [ _ | RevPath ]) ->
    relpath_1(Ps, RevPath);
relpath_1([ P ], RevPath) ->
    {P, RevPath};
relpath_1([ Dir | Ps ], RevPath) ->
    relpath_1(Ps, [ Dir | RevPath ]).

% Make basenames of hrefs into links to resources.
% Use the same patterns as the name generation.
map_link(Filename, RevPath) ->
    Filename1 = binary:replace(Filename, <<"-">>, <<"_">>, [ global ]),
    case filename_to_name(Filename1, RevPath) of
        error ->
            io:format("No link mapping for: ~s in ~p~n", [ Filename, RevPath ]),
            error;
        {Name, _Cat, _IsAlways} ->
            <<"/id/", Name/binary>>
    end.

% Elements that shouldn't use a open and close tag.
is_selfclosing(<<"br">>) -> true;
is_selfclosing(<<"hr">>) -> true;
is_selfclosing(<<"img">>) -> true;
is_selfclosing(_) -> false.

% Check if the attribute might contain an url
is_url_attr(<<"src">>) -> true;
is_url_attr(<<"href">>) -> true;
is_url_attr(<<"poster">>) -> true;
is_url_attr(_) -> false.

% Sphinx adds odd/even to table rows. This site uses
% for the odd/even markup, so remove the class from the HTML.
filter_class(<<"row-odd">>) -> <<>>;
filter_class(<<"row-even">>) -> <<>>;
filter_class(Class) ->
    Class.


% Extract extra properties from the HTML parse tree. For example
% links to modules, and other documents.
extract_props(Html, RevPath) ->
    extract(Html, RevPath, #{
            in_module => [],
            links => [],
            seealso => [],
            toctree => [],
            other => []
        }).

extract({<<"a">>, Attrs, Elts}, RevPath, #{ links := Links } = Acc) ->
    Acc1 = extract(Elts, RevPath, Acc),
    case proplists:get_value(<<"href">>, Attrs) of
        undefined ->
            Acc1;
        Href ->
            case fix_link(<<"href">>, Href, RevPath) of
                <<"/id/", _/binary>> = IdRef ->
                    [ NewLink | _ ] = binary:split(IdRef, <<"#">>),
                    Acc1#{
                        links => [ NewLink | Links ]
                    };
                _ ->
                    Acc1
            end
    end;
extract({_Elt, Attrs, Elts}, RevPath, Acc) ->
    Class = proplists:get_value(<<"class">>, Attrs, <<>>),
    Classes = binary:split(Class, <<" ">>, [global]),
    Acc1 = extract_first([
            fun extract_toctree/5,
            fun extract_seealso/5,
            fun extract_in_module/5
        ],
        Classes, Attrs, Elts, RevPath, Acc),
    Acc1;
extract(L, RevPath, Acc) when is_list(L) ->
    lists:foldl( fun(E, As) -> extract(E, RevPath, As) end, Acc, L);
extract(_, _RevPath, Acc) ->
    Acc.

%% Extract information from special elements.
extract_first([], _Classes, _Attrs, Elts, RevPath, Acc) ->
    extract(Elts, RevPath, Acc);
extract_first([ F | Fs ], Classes, Attrs, Elts, RevPath, Acc) ->
    case F(Classes, Attrs, Elts, RevPath, Acc) of
        {ok, Acc1} ->
            Acc1;
        false ->
            extract_first(Fs, Classes, Attrs, Elts, RevPath, Acc)
    end.


extract_toctree(Classes, _Attrs, Elts, RevPath, Acc) ->
    #{
        toctree := TocTreeAcc,
        links := LinksAcc,
        other := OtherAcc
    } = Acc,
    case lists:member(<<"toctree-l1">>, Classes) of
        true ->
            #{
                links := Links,
                other := Other
            } = extract_props(Elts, RevPath),
            Acc1 = Acc#{
                toctree => Links ++ TocTreeAcc,
                links => Links ++ Other ++ LinksAcc
            },
            {ok, Acc1};
        false ->
            case lists:member(<<"toctree-l2">>, Classes)
                orelse lists:member(<<"toctree-l3">>, Classes)
            of
                true ->
                    #{ links := Links } = extract_props(Elts, RevPath),
                    Acc1 = Acc#{
                        other => Links ++ OtherAcc
                    },
                    {ok, Acc1};
                false ->
                    false
            end
    end.

% Seealso links are grouped in an "admonition-see-also" class div
extract_seealso(Classes, _Attrs, Elts, RevPath, #{ seealso := SeeAlsoAcc } = Acc) ->
    case lists:member(<<"admonition-see-also">>, Classes)
        orelse lists:member(<<"admonition">>, Classes)
    of
        true ->
            #{ links := Links } = extract_props(Elts, RevPath),
            Acc1 = Acc#{ seealso => Links ++ SeeAlsoAcc },
            {ok, Acc1};
        false ->
            false
    end;
extract_seealso(_Classes, _Attrs, _Elts, _RevPath, _Acc) ->
    false.

extract_in_module(Classes, _Attrs, Elts, _RevPath, #{ in_module := InMod } = Acc) ->
    case lists:member(<<"simple">>, Classes) of
        true ->
            % Could be a reference to the module
            case text(Elts) of
                <<"Module: core">> ->
                    {ok, Acc#{
                        in_module => [ <<"/id/doc_core">> | InMod ]
                    }};
                <<"Module: ", Module/binary>> ->
                    Module1 = z_string:trim(Module),
                    {ok, Acc#{
                        in_module => [ <<"/id/doc_module_", Module1/binary>> | InMod ]
                    }};
                _ ->
                    false
            end;
        false ->
            false
    end;
extract_in_module(_Classes, _Attrs, _Elts, _RevPath, _Acc) ->
    false.


%% @doc Delete all imported documentation.... only run manually
% in development of the zotonic site as it will delete *everything*.
-spec cleanup_do_not_run( z:context() ) -> {ok, non_neg_integer()}.
cleanup_do_not_run(Context) ->
    % Crash if the current user is not an administrator
    true = z_acl:is_admin(Context),
    Count = z_db:q("
        delete from rsc
        where name like 'doc_%'
          and not id in (select id from protect)",
        Context),
    %
    % Delete all remaining edges made by the Gitbot.
    GitUserId = m_rsc:rid(gitbot, Context),
    z_db:q("
        delete from edge
        where creator_id = $1",
        [ GitUserId ],
        Context),
    % Reset all caches - as we deleted data from the database
    % without telling the caching systems.
    z:flush(Context),
    {ok, Count}.

-spec cleanup_edges_do_not_run( z:context() ) -> {ok, non_neg_integer()}.
cleanup_edges_do_not_run(Context) ->
    % Crash if the current user is not an administrator
    true = z_acl:is_admin(Context),
    %
    % Delete all remaining edges made by the Gitbot.
    GitUserId = m_rsc:rid(gitbot, Context),
    Count = z_db:q("
        delete from edge
        where creator_id = $1",
        [ GitUserId ],
        Context),
    % Reset all caches - as we deleted data from the database
    % without telling the caching systems.
    z:flush(Context),
    {ok, Count}.


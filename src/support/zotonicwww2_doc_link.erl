%% @doc Render documentation Markdown and link Zotonic reference code spans.
%%
%% References use the form `kind#name`, for example `tag#print` or
%% `module#mod_base`. Only inline code spans are considered. Code blocks,
%% existing links, and Erlang documentation references are left unchanged.
%% @end

-module(zotonicwww2_doc_link).

-behaviour(markdownz_plugin).

-export([
    to_html/1,
    to_html_document/1,
    init/2,
    link_references/2
]).

-type html_element() ::
    binary()
    | {'=', binary()}
    | {binary(), [{binary(), term()}], [html_element()]}.


%% @doc Convert Markdown to HTML and link recognized Zotonic references.
-spec to_html(unicode:chardata()) -> binary().
to_html(Markdown) ->
    Config = markdownz:use(markdownz:new(), ?MODULE),
    markdownz:to_binary(Markdown, Config).


%% @doc Convert a Markdown document while retaining its optional front matter.
-spec to_html_document(unicode:chardata()) ->
    {ok, markdownz:document(binary())}
    | {error, term(), term()}
    | {incomplete, binary(), term()}.
to_html_document(Markdown) ->
    case markdownz:split_document(Markdown) of
        {ok, #{content := Content} = Document} ->
            {ok, Document#{content := to_html(Content)}};
        {error, _, _} = Error ->
            Error;
        {incomplete, _, _} = Error ->
            Error
    end.


%% @doc Install the Zotonic reference resolver as the final Markdown tree pass.
-spec init(markdownz:config(), map()) -> markdownz:config().
init(Config, _Options) ->
    markdownz:add_rule(
        Config,
        core,
        'after',
        task_lists,
        {zotonic_references, {?MODULE, link_references}}).


%% @doc Resolve Zotonic references in a parsed Markdown tree.
-spec link_references([html_element()], map()) -> {[html_element()], map()}.
link_references(Tree, State) ->
    {link_references(Tree), State}.


-spec link_references([html_element()]) -> [html_element()].
link_references(Nodes) ->
    [ link_reference(Node) || Node <- Nodes ].

link_reference({<<"pre">>, _, _} = Pre) ->
    Pre;
link_reference({<<"a">>, _, _} = Link) ->
    Link;
link_reference({<<"code">>, _, [Reference]} = Code) when is_binary(Reference) ->
    case reference_page(Reference) of
        {ok, Kind, PageName} ->
            {<<"a">>, [
                {<<"href">>, <<"/id/", PageName/binary>>},
                {<<"class">>, <<"doc-reference doc-reference-", Kind/binary>>}
            ], [Code]};
        error ->
            Code
    end;
link_reference({Tag, Attrs, Children}) ->
    {Tag, Attrs, link_references(Children)};
link_reference(Text) ->
    Text.


reference_page(Reference) ->
    case binary:split(Reference, <<"#">>) of
        [Kind, Name] -> reference_page(Kind, Name);
        [_] -> error
    end.

reference_page(<<"tag">> = Kind, Name) ->
    reference(Kind, <<"doc_template_tag_tag_">>, Name);
reference_page(<<"filter">> = Kind, Name) ->
    reference(Kind, <<"doc_template_filter_filter_">>, Name);
reference_page(<<"scomp">> = Kind, Name) ->
    reference(Kind, <<"doc_template_scomp_scomp_">>, Name);
reference_page(<<"action">> = Kind, Name) ->
    reference(Kind, <<"doc_template_action_action_">>, Name);
reference_page(<<"validator">> = Kind, Name) ->
    reference(Kind, <<"doc_template_validator_validator_">>, Name);
reference_page(<<"model">> = Kind, Name) ->
    reference(Kind, <<"doc_model_model_">>, Name);
reference_page(<<"controller">> = Kind, Name) ->
    reference(Kind, <<"doc_controller_">>, Name);
reference_page(<<"module">> = Kind, <<"zotonic_core">>) ->
    {ok, Kind, <<"doc_core">>};
reference_page(<<"module">> = Kind, Name) ->
    reference(Kind, <<"doc_module_">>, Name);
reference_page(<<"notification">> = Kind, Name) ->
    reference(Kind, <<"doc_notification_">>, Name);
reference_page(<<"dispatch">> = Kind, Name) ->
    dispatch_reference(Kind, binary:split(Name, <<"/">>, [global]));
reference_page(_Kind, _Name) ->
    error.

reference(Kind, Prefix, Name) ->
    case is_name(Name) of
        true -> {ok, Kind, <<Prefix/binary, Name/binary>>};
        false -> error
    end.

dispatch_reference(Kind, [Module, Filename]) ->
    case is_name(Module) andalso is_name(Filename) of
        true ->
            PageName = z_string:to_name(
                <<"doc_dispatch_dispatch_", Module/binary, "_", Filename/binary>>),
            {ok, Kind, PageName};
        false ->
            error
    end;
dispatch_reference(_Kind, _Parts) ->
    error.

is_name(<<>>) ->
    false;
is_name(Name) ->
    is_name_chars(Name).

is_name_chars(<<>>) ->
    true;
is_name_chars(<<C, Rest/binary>>)
    when C >= $a, C =< $z;
         C >= $0, C =< $9;
         C =:= $_;
         C =:= $- ->
    is_name_chars(Rest);
is_name_chars(_) ->
    false.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reference_mapping_test_() ->
    References = [
        {<<"tag#print">>, <<"doc_template_tag_tag_print">>},
        {<<"filter#escape">>, <<"doc_template_filter_filter_escape">>},
        {<<"scomp#wire">>, <<"doc_template_scomp_scomp_wire">>},
        {<<"action#update">>, <<"doc_template_action_action_update">>},
        {<<"validator#presence">>, <<"doc_template_validator_validator_presence">>},
        {<<"model#rsc">>, <<"doc_model_model_rsc">>},
        {<<"controller#controller_page">>, <<"doc_controller_controller_page">>},
        {<<"module#mod_base">>, <<"doc_module_mod_base">>},
        {<<"module#zotonic_core">>, <<"doc_core">>},
        {<<"notification#media_upload">>, <<"doc_notification_media_upload">>},
        {<<"dispatch#mod_base/dispatch-cotonic">>,
            <<"doc_dispatch_dispatch_mod_base_dispatch_cotonic">>}
    ],
    [
        ?_assertMatch(
            {_, _},
            binary:match(to_html(<<"`", Reference/binary, "`">>),
                <<"href=\"/id/", PageName/binary, "\"">>))
        || {Reference, PageName} <- References
    ].

non_reference_code_test() ->
    Html = to_html(<<"`unknown#thing` and `z_template:render/3`">>),
    ?assertEqual(nomatch, binary:match(Html, <<"<a ">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"<code>unknown#thing</code>">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"<code>z_template:render/3</code>">>)).

unsafe_reference_is_not_linked_test() ->
    Html = to_html(<<"`tag#\"><script>`">>),
    ?assertEqual(nomatch, binary:match(Html, <<"<a ">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"&lt;script&gt;">>)).

code_block_is_not_linked_test() ->
    Html = to_html(<<"```django\n`tag#print`\n```">>),
    ?assertEqual(nomatch, binary:match(Html, <<"<a ">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"<pre">>)).

existing_link_is_not_nested_test() ->
    Html = to_html(<<"[`tag#print`](/id/original)">>),
    ?assertEqual(1, length(binary:matches(Html, <<"<a ">>))),
    ?assertNotEqual(nomatch, binary:match(Html, <<"href=\"/id/original\"">>)).

front_matter_test() ->
    {ok, #{front_matter := FrontMatter, content := Html}} = to_html_document(
        <<"---\nkeywords: [render]\n---\nUse `tag#print`.\n">>),
    ?assertMatch(#{format := yaml}, FrontMatter),
    ?assertNotEqual(nomatch, binary:match(Html, <<"href=\"/id/doc_template_tag_tag_print\"">>)).

-endif.

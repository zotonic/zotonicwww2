%% @doc Omit a leading body heading when the page already renders that title.
-module(filter_zotonicwww2_without_title).
-moduledoc("Remove a leading H1 matching the resource title before rendering a documentation body and its table of contents.").

-export([zotonicwww2_without_title/3]).

-spec zotonicwww2_without_title(Body, Title, Context) -> Result when
    Body :: term(), Title :: term(), Context :: z:context(), Result :: binary().
zotonicwww2_without_title(Body, Title, Context) ->
    Html = z_convert:to_binary(z_trans:lookup_fallback(Body, Context)),
    PageTitle = z_trans:lookup_fallback(Title, Context),
    strip_title(Html, PageTitle).

strip_title(Html, Title) ->
    case re:run(Html, <<"^\\s*<h1(?:\\s[^>]*)?>(.*?)</h1>\\s*">>,
                [dotall, {capture, [0, 1], binary}]) of
        {match, [Heading, Text]} ->
            case normalized_text(Text) =:= normalized_text(Title) of
                true ->
                    %% The match ends at an HTML boundary, never inside UTF-8.
                    Size = byte_size(Heading),
                    <<_:Size/binary, Rest/binary>> = Html,
                    Rest;
                false -> Html
            end;
        nomatch -> Html
    end.

normalized_text(Html) ->
    Text = z_string:trim(z_html:unescape(z_html:strip(Html))),
    re:replace(Text, <<"\\s+">>, <<" ">>, [unicode, global, {return, binary}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

matching_title_test() ->
    ?assertEqual(<<"<p>Events.</p><h2>event</h2>">>,
        strip_title(<<"<h1>model/activity</h1>\n<p>Events.</p><h2>event</h2>">>,
                    <<"model/activity">>)),
    ?assertEqual(<<"<p>Body.</p>">>,
        strip_title(<<"\n<h1><code>A &amp; B</code></h1><p>Body.</p>">>, <<"A &amp; B">>)).

other_content_test() ->
    lists:foreach(fun(Html) -> ?assertEqual(Html, strip_title(Html, <<"Title">>)) end,
        [<<"<h1>Different</h1><p>Body.</p>">>,
         <<"<p>Introduction.</p><h1>Title</h1>">>,
         <<"<h2>Title</h2>">>, <<>>]).
-endif.

%% @doc Extract metadata from Zotonic release-note Markdown.
%%
%% Release notes have used a few different introductory sentence formats over
%% the years. Only dates explicitly following "released on" are accepted;
%% unrelated dates elsewhere in the notes are ignored.
%% @end

-module(zotonicwww2_release_notes).

-export([
    org_pubdate/1
]).


-spec org_pubdate(binary()) -> calendar:datetime() | undefined.
org_pubdate(Markdown) when is_binary(Markdown) ->
    case captures(Markdown, "released\\s+on\\s+([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})") of
        {ok, [Year, Month, Day]} ->
            valid_date(Year, Month, Day);
        nomatch ->
            org_pubdate_textual(Markdown)
    end.

org_pubdate_textual(Markdown) ->
    case captures(
        Markdown,
        "released\\s+on\\s+([0-9]{1,2})(?:st|nd|rd|th)?\\s+([A-Za-z]+)\\.?\\s*,?\\s+([0-9]{4})")
    of
        {ok, [Day, MonthName, Year]} ->
            valid_date(Year, month_number(MonthName), Day);
        nomatch ->
            org_pubdate_month_first(Markdown)
    end.

org_pubdate_month_first(Markdown) ->
    case captures(
        Markdown,
        "released\\s+on\\s+([A-Za-z]+)\\.?\\s+([0-9]{1,2})(?:st|nd|rd|th)?\\s*,?\\s+([0-9]{4})")
    of
        {ok, [MonthName, Day, Year]} ->
            valid_date(Year, month_number(MonthName), Day);
        nomatch ->
            undefined
    end.

captures(Markdown, Pattern) ->
    case re:run(Markdown, Pattern, [ caseless, {capture, all_but_first, binary} ]) of
        {match, Captures} -> {ok, Captures};
        nomatch -> nomatch
    end.

valid_date(_Year, undefined, _Day) ->
    undefined;
valid_date(YearBin, Month, DayBin) ->
    Date = {
        binary_to_integer(YearBin),
        to_integer(Month),
        binary_to_integer(DayBin)
    },
    case calendar:valid_date(Date) of
        true -> {Date, {0, 0, 0}};
        false -> undefined
    end.

to_integer(Value) when is_integer(Value) -> Value;
to_integer(Value) when is_binary(Value) -> binary_to_integer(Value).

month_number(Month) ->
    case string:lowercase(Month) of
        <<"jan">> -> 1;
        <<"january">> -> 1;
        <<"feb">> -> 2;
        <<"february">> -> 2;
        <<"mar">> -> 3;
        <<"march">> -> 3;
        <<"apr">> -> 4;
        <<"april">> -> 4;
        <<"may">> -> 5;
        <<"jun">> -> 6;
        <<"june">> -> 6;
        <<"jul">> -> 7;
        <<"july">> -> 7;
        <<"aug">> -> 8;
        <<"august">> -> 8;
        <<"sep">> -> 9;
        <<"sept">> -> 9;
        <<"september">> -> 9;
        <<"oct">> -> 10;
        <<"october">> -> 10;
        <<"nov">> -> 11;
        <<"november">> -> 11;
        <<"dec">> -> 12;
        <<"december">> -> 12;
        _ -> undefined
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

org_pubdate_test_() ->
    [
        ?_assertEqual(
            {{2011, 8, 3}, {0, 0, 0}},
            org_pubdate(<<"Released on 2011-08-03 13:17 by arjan.">>)),
        ?_assertEqual(
            {{2017, 7, 3}, {0, 0, 0}},
            org_pubdate(<<"Welcome to Zotonic, released on 3 July, 2017.">>)),
        ?_assertEqual(
            {{2015, 7, 3}, {0, 0, 0}},
            org_pubdate(<<"Welcome Zotonic, released on July 3, 2015.">>)),
        ?_assertEqual(
            {{2021, 10, 29}, {0, 0, 0}},
            org_pubdate(<<"Welcome to Zotonic, released on Oct 29, 2021.">>)),
        ?_assertEqual(
            {{2016, 2, 29}, {0, 0, 0}},
            org_pubdate(<<"Welcome, released on 29th February 2016.">>)),
        ?_assertEqual(
            undefined,
            org_pubdate(<<"Released on February 29, 2015.">>)),
        ?_assertEqual(undefined, org_pubdate(<<"See the 2025-01-01 archive for details.">>)),
        ?_assertEqual(undefined, org_pubdate(<<"No release date yet.">>))
    ].

-endif.

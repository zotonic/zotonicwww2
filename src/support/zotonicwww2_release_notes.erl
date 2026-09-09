%% @doc Validate release-note metadata read from Markdown front matter.
%% @end

-module(zotonicwww2_release_notes).

-export([
    release_date/1
]).


-spec release_date(binary() | undefined) -> calendar:datetime() | undefined.
release_date(undefined) ->
    undefined;
release_date(Date) when is_binary(Date) ->
    case re:run(
        Date,
        <<"^([0-9]{4})-([0-9]{2})-([0-9]{2})$">>,
        [{capture, all_but_first, binary}])
    of
        {match, [Year, Month, Day]} ->
            DateTuple = {
                binary_to_integer(Year),
                binary_to_integer(Month),
                binary_to_integer(Day)
            },
            case calendar:valid_date(DateTuple) of
                true -> {DateTuple, {0, 0, 0}};
                false -> erlang:error({invalid_release_date, Date})
            end;
        nomatch ->
            erlang:error({invalid_release_date, Date})
    end;
release_date(Invalid) ->
    erlang:error({invalid_release_date, Invalid}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

release_date_test_() ->
    [
        ?_assertEqual(
            {{2026, 8, 25}, {0, 0, 0}},
            release_date(<<"2026-08-25">>)),
        ?_assertEqual(undefined, release_date(undefined)),
        ?_assertError(
            {invalid_release_date, <<"2025-02-29">>},
            release_date(<<"2025-02-29">>)),
        ?_assertError(
            {invalid_release_date, <<"25 August 2026">>},
            release_date(<<"25 August 2026">>))
    ].

-endif.

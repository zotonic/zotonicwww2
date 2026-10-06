%% @doc Validate release-note metadata read from Markdown front matter.
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

-module(zotonicwww2_release_notes).
-moduledoc("
Validate release-note metadata read from Markdown front matter.

`release_date/1` accepts an ISO date in `YYYY-MM-DD` form and returns a
calendar datetime at midnight. Missing dates return `undefined`; malformed
or impossible dates raise `{invalid_release_date, Value}`.
").

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

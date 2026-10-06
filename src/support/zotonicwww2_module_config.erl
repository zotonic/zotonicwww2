%% @doc Read declared module configuration without loading the compiled module.

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

-module(zotonicwww2_module_config).
-moduledoc("
Read declared module configuration without loading the compiled module.

`from_beam/1` reads `-mod_config` attributes from a BEAM file and returns
normalized configuration rows for storage on a module documentation page.
Normalization is shared with the external source parser. Missing defaults
remain distinguishable from explicit defaults such as `false` or `undefined`.
").
-export([from_beam/1]).
-include("../../priv/bin/module_config.hrl").

-spec from_beam(file:filename_all()) -> [map()].
from_beam(Filename) ->
    case beam_lib:chunks(unicode:characters_to_list(Filename), [attributes]) of
        {ok, {Module, [{attributes, Attributes}]}} ->
            Configs = lists:append(proplists:get_all_values(mod_config, Attributes)),
            config_rows(Module, Configs);
        {error, beam_lib, Reason} ->
            error({module_config, Filename, Reason})
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
config_rows_test() ->
    [A, B, C, D] = config_rows(mod_example, [
        #{key => enabled, type => boolean, default => false, description => "Enable <example>."},
        #{module => site, key => title, default => <<>>},
        #{name => legacy, default => #{mode => [one, two]}},
        #{key => unspecified}
    ]),
    ?assertMatch(#{<<"module">> := <<"mod_example">>, <<"key">> := <<"enabled">>,
        <<"type">> := <<"boolean">>, <<"default">> := <<"false">>,
        <<"has_default">> := true, <<"description">> := <<"Enable <example>.">>}, A),
    ?assertMatch(#{<<"module">> := <<"site">>, <<"default">> := <<"<<>>">>}, B),
    ?assertEqual(<<"legacy">>, maps:get(<<"key">>, C)),
    ?assertEqual(config_term(#{mode => [one, two]}), maps:get(<<"default">>, C)),
    ?assertMatch(#{<<"default">> := <<>>, <<"has_default">> := false}, D),
    ?assertEqual([], config_rows(mod_example, [])).
-endif.

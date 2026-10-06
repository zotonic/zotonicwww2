%% Shared, dependency-free normalization for BEAM attributes and the isolated
%% source parser. Return only binary-keyed maps and scalar display values;
%% arbitrary repository atoms must never enter the site's VM.
config_rows(Module, Configs) when is_list(Configs) ->
    [config_row(Module, Config) || Config <- Configs].

config_row(Module, Config) when is_map(Config) ->
    Key = case maps:find(key, Config) of
        {ok, K} -> K;
        error -> maps:get(name, Config)
    end,
    #{
        <<"module">> => config_text(maps:get(module, Config, Module)),
        <<"key">> => config_text(Key),
        <<"type">> => config_text(maps:get(type, Config, <<>>)),
        <<"has_default">> => maps:is_key(default, Config),
        <<"default">> => case maps:find(default, Config) of
            {ok, Default} -> config_term(Default);
            error -> <<>>
        end,
        <<"description">> => config_text(maps:get(description, Config, <<>>))
    }.

config_text(Value) when is_atom(Value) -> atom_to_binary(Value);
config_text(Value) when is_binary(Value) -> Value;
config_text(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        _ -> config_term(Value)
    catch
        error:badarg -> config_term(Value)
    end;
config_text(Value) -> config_term(Value).

%% Erlang syntax distinguishes false, undefined, empty strings/binaries, lists,
%% and structured defaults. These are declarations, never live config values.
config_term(Value) -> unicode:characters_to_binary(io_lib:format("~tp", [Value])).

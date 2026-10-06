#!/usr/bin/env escript
%%! +S 1:1 +A 1 +hmax 16777216 -noshell
%% Parse untrusted sources in a disposable VM: never compile, load, preprocess,
%% or evaluate repository code. Only binary-keyed data crosses into Zotonic.
-mode(interpret).
-include_lib("kernel/include/file.hrl").
-include("module_config.hrl").

main([Root, Output]) ->
    Files = files(Root, "."),
    true = length(Files) =< 2000,
    Rows = parse_files(Root, Files, 0, []),
    ok = file:write_file(Output, term_to_binary(Rows)).

parse_files(_Root, [], _Bytes, Acc) -> lists:reverse(Acc);
parse_files(Root, [Path | Rest], Bytes, Acc) ->
    Row = parse(Root, Path),
    Total = Bytes + byte_size(term_to_binary(Row)),
    true = Total =< 16777216,
    parse_files(Root, Rest, Total, [Row | Acc]).

files(Root, Relative) ->
    {ok, Names} = file:list_dir(filename:join(Root, Relative)),
    lists:append([file_entry(Root, filename:join(Relative, N)) || N <- lists:sort(Names), N =/= ".git"]).

file_entry(Root, Path) ->
    case file:read_link_info(filename:join(Root, Path)) of
        {ok, #file_info{type = directory}} -> files(Root, Path);
        {ok, #file_info{type = regular}} ->
            case is_dispatch_file(Path) of
                true -> [{dispatch, Path}];
                false ->
                    case filename:extension(Path) of ".erl" -> [{erlang, Path}]; _ -> [] end
            end;
        _ -> []
    end.

is_dispatch_file(Path) ->
    Dir = filename:dirname(Path),
    Name = filename:basename(Path),
    filename:basename(Dir) =:= "dispatch"
        andalso filename:basename(filename:dirname(Dir)) =:= "priv"
        andalso hd(Name) =/= $.
        andalso hd(Name) =/= $#
        andalso lists:last(Name) =/= $~
        andalso not lists:member(filename:extension(Name), [".bak", ".swp", ".swo", ".tmp"]).

parse(Root, {Kind, Path}) ->
    Base = #{<<"path">> => unicode:characters_to_binary(Path), <<"kind">> => atom_to_binary(Kind)},
    try
        Full = filename:join(Root, Path),
        {ok, #file_info{size = Size}} = file:read_file_info(Full),
        true = Size =< 2097152,
        {ok, Data} = file:read_file(Full),
        Encoding = case epp:read_encoding(Full) of none -> utf8; E -> E end,
        Chars = unicode:characters_to_list(Data, Encoding),
        {ok, Tokens, _} = erl_scan:string(Chars),
        Parsed = case Kind of
            erlang -> parse_source(Root, Path, Tokens);
            dispatch -> #{<<"rules">> => dispatch_rules(Tokens)}
        end,
        maps:merge(Base, Parsed#{<<"status">> => <<"parsed">>})
    catch
        _:Reason -> Base#{<<"status">> => <<"skipped">>,
                          <<"error">> => unicode:characters_to_binary(io_lib:format("~P", [Reason, 8]))}
    end.

parse_source(Root, Path, Tokens) ->
    Attrs = attributes(Tokens, [], []),
    [Module] = [M || {module, M} <- Attrs, is_atom(M)],
    Docs = [D || {moduledoc, D} <- Attrs, not is_map(D)],
    Doc = case Docs of
        [D] when is_list(D); is_binary(D) -> unicode:characters_to_binary(D);
        [{file, DocPath}] when is_list(DocPath) -> read_doc(Root, Path, DocPath);
        [] -> throw(missing_moduledoc);
        [false] -> throw(hidden_moduledoc);
        _ -> throw(unsupported_moduledoc)
    end,
    case string:trim(Doc) of <<>> -> throw(missing_moduledoc); _ -> ok end,
    Keywords = lists:append([maps:get(zotonic_keywords, M, []) || {moduledoc, M} <- Attrs, is_map(M)]),
    true = is_list(Keywords),
    Slugs = [keyword(K) || K <- Keywords],
    Configs = lists:append([Cs || {mod_config, Cs} <- Attrs]),
    #{<<"config">> => config_rows(Module, Configs),
      <<"observes">> => observer_notifications(Attrs),
      <<"module">> => atom_to_binary(Module), <<"doc">> => Doc,
      <<"keywords">> => lists:usort(Slugs)}.

%% Match the callback names and arities registered by z_module_manager.
%% Only exported functions count; all notification names leave this VM as text.
observer_notifications(Attrs) ->
    Exports = lists:append([Es || {export, Es} <- Attrs]),
    lists:usort([
        Name
        || {Fun, Arity} <- Exports,
           is_atom(Fun),
           Name <- [observer_notification(atom_to_binary(Fun), Arity)],
           Name =/= undefined
    ]).

observer_notification(<<"observe_", Name/binary>>, Arity)
    when Name =/= <<>>, (Arity =:= 2 orelse Arity =:= 3) -> Name;
observer_notification(<<"pid_observe_", Name/binary>>, Arity)
    when Name =/= <<>>, (Arity =:= 3 orelse Arity =:= 4) -> Name;
observer_notification(_, _) -> undefined.

keyword(K) when is_list(K); is_binary(K) ->
    B = unicode:characters_to_binary(K),
    true = is_binary(B) andalso byte_size(B) > 0,
    B;
keyword(_) -> throw(invalid_keywords).

%% Split scanner forms, parsing literal documentation, config, and export attributes.
%% Function macros and unavailable include files do not obstruct documentation.
attributes([], [], Acc) -> lists:reverse(Acc);
attributes([], _Partial, _Acc) -> throw(incomplete_form);
attributes([{dot, _} = Dot | Rest], Form, Acc) ->
    Tokens = lists:reverse([Dot | Form]),
    Next = case Tokens of
        [{'-', _}, {atom, _, Name} | _] when Name =:= module; Name =:= moduledoc; Name =:= mod_config; Name =:= export ->
            case erl_parse:parse_form(Tokens) of
                {ok, {attribute, _, Name, Value}} -> [{Name, Value} | Acc];
                {error, Error} -> throw({invalid_attribute, Error})
            end;
        _ -> Acc
    end,
    attributes(Rest, [], Next);
attributes([T | Rest], Form, Acc) -> attributes(Rest, [T | Form], Acc).

%% Resolve a file-backed moduledoc relative to the source file, as epp does,
%% but forbid escaping the checkout or following any symlink along the path.
read_doc(Root, Source, DocPath) ->
    relative = filename:pathtype(DocPath),
    Parts = normalize_parts(filename:split(filename:join(filename:dirname(Source), DocPath)), []),
    Full = safe_path(Root, Parts),
    {ok, #file_info{type = regular, size = Size}} = file:read_link_info(Full),
    true = Size =< 2097152,
    {ok, Data} = file:read_file(Full),
    Doc = unicode:characters_to_binary(Data),
    true = is_binary(Doc),
    Doc.

normalize_parts([], Acc) -> lists:reverse(Acc);
normalize_parts(["." | Rest], Acc) -> normalize_parts(Rest, Acc);
normalize_parts([".." | Rest], [_ | Acc]) -> normalize_parts(Rest, Acc);
normalize_parts([".." | _], []) -> throw(document_outside_repository);
normalize_parts([Part | Rest], Acc) -> normalize_parts(Rest, [Part | Acc]).

safe_path(Path, []) -> Path;
safe_path(Path, [Part | Rest]) ->
    Next = filename:join(Path, Part),
    {ok, #file_info{type = Type}} = file:read_link_info(Next),
    true = Type =:= directory orelse Type =:= regular,
    safe_path(Next, Rest).


%% Dispatch files contain literal Erlang terms, not expressions. Parsing terms
%% cannot call functions. Atoms are converted to display binaries in this VM.
dispatch_rules(Tokens) ->
    Terms = dispatch_terms(Tokens, [], []),
    Rules = lists:flatten(Terms),
    [dispatch_rule(R) || R <- Rules].

dispatch_terms([], [], Acc) -> lists:reverse(Acc);
dispatch_terms([], _, _) -> throw(incomplete_dispatch_term);
dispatch_terms([{dot, _} = Dot | Rest], Form, Acc) ->
    case erl_parse:parse_term(lists:reverse([Dot | Form])) of
        {ok, Term} -> dispatch_terms(Rest, [], [Term | Acc]);
        {error, Error} -> throw({invalid_dispatch_term, Error})
    end;
dispatch_terms([T | Rest], Form, Acc) -> dispatch_terms(Rest, [T | Form], Acc).

dispatch_rule({Name, Path, Controller, Options})
    when is_atom(Name), is_list(Path), is_atom(Controller), is_list(Options) ->
    #{<<"name">> => atom_to_binary(Name), <<"path">> => dispatch_path(Path),
      <<"controller">> => atom_to_binary(Controller), <<"options">> => config_term(Options)};
dispatch_rule(_) -> throw(invalid_dispatch_rule).

dispatch_path([]) -> <<"/">>;
dispatch_path([C | _] = Path) when is_integer(C) -> unicode:characters_to_binary(Path);
dispatch_path(Path) ->
    iolist_to_binary([<<"/">>, lists:join(<<"/">>, [dispatch_segment(S) || S <- Path])]).

dispatch_segment('*') -> <<"*">>;
dispatch_segment(S) when is_atom(S) -> <<":", (atom_to_binary(S))/binary>>;
dispatch_segment(S) when is_binary(S) -> S;
dispatch_segment(S) when is_list(S) -> unicode:characters_to_binary(S);
dispatch_segment({Variable, Match}) when is_atom(Variable) ->
    <<":", (atom_to_binary(Variable))/binary, "=", (config_term(Match))/binary>>;
dispatch_segment(S) -> config_term(S).

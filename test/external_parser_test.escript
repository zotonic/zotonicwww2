#!/usr/bin/env escript
%%! +S 1:1 +A 1 -noshell
%% Run from the site directory: escript test/external_parser_test.escript
-mode(compile).
main(_) ->
    Root = filename:join("/tmp", "external-parser-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Root),
    Output = Root ++ ".etf",
    try
        write(Root, "mod_ok.erl", <<"-module(mod_ok).\n-include_lib(\"missing.hrl\").\n-moduledoc \"\"\"\n# Hello\n\nDocumented.\n\"\"\".\n-moduledoc(#{zotonic_keywords => [\"configure\"]}).\nf() -> ?UNDEFINED_MACRO.\n">>),
        {ok, File} = file:open(filename:join(Root, "mod_ok.erl"), [append]),
        ok = file:write(File, <<"-mod_config([#{key => enabled, type => boolean, default => false, description => \"Enable <example>.\"}, #{module => site, key => title, default => <<>>}]).\n-mod_config([#{name => legacy, default => #{mode => [one, two]}}, #{key => missing_default}]).\n">>),
        ok = file:write(File, <<"-export([observe_demo/2, observe_demo/3, pid_observe_demo/3, observe_invalid/1, helper/2]).\n-export([pid_observe_fold/4, pid_observe_invalid/2, observe_/2]).\nobserve_private(A, B) -> {A, B}.\n">>),
        ok = file:close(File),
        write(Root, "m_plain.erl", <<"-module(m_plain).\n-moduledoc(\"Plain docs\").\n">>),
        write(Root, "missing.erl", <<"-module(missing).\n">>),
        write(Root, "hidden.erl", <<"-module(hidden).\n-moduledoc(false).\n">>),
        write(Root, "empty.erl", <<"-module(empty).\n-moduledoc(\"  \" ).\n">>),
        write(Root, "broken.erl", <<"-module(broken).\n-moduledoc(\"unterminated).\n">>),
        write(Root, "macro.erl", <<"-module(macro).\n-moduledoc(?DOC).\n">>),
        write(Root, "danger.erl", <<"-module(danger).\n-on_load(run/0).\n-moduledoc(\"No evaluation\").\nrun() -> erlang:halt(99).\n">>),
        ok = file:make_symlink("/etc/passwd", filename:join(Root, "symlink.erl")),
        ok = file:make_dir(filename:join(Root, "nested")),
        write(Root, "nested/mod_nested.erl", <<"-module(mod_nested).\n-moduledoc(\"Nested\").\n">>),
        write(Root, "README.md", <<"File documentation">>),
        write(Root, "nested/file_doc.erl", <<"-module(file_doc).\n-moduledoc({file, \"../README.md\"}).\n">>),
        write(Root, "escape.erl", <<"-module(escape).\n-moduledoc({file, \"../../../etc/passwd\"}).\n">>),
        write(Root, "link.erl", <<"-module(link).\n-moduledoc({file, \"symlink.erl\"}).\n">>),
        Port = open_port({spawn_executable, os:find_executable("escript")},
            [exit_status, {args, ["priv/bin/parse_external_docs.escript", Root, Output]}]),
        receive {Port, {exit_status, 0}} -> ok after 15000 -> error(parser_failed) end,
        {ok, Bin} = file:read_file(Output),
        Rows = binary_to_term(Bin, [safe]),
        12 = length(Rows),
        Imported = [R || #{<<"status">> := <<"parsed">>} = R <- Rows],
        5 = length(Imported),
        [#{<<"keywords">> := [<<"configure">>], <<"doc">> := <<"# Hello\n\nDocumented.">>}] =
            [R || #{<<"module">> := <<"mod_ok">>} = R <- Imported],
        [#{<<"observes">> := [<<"demo">>, <<"fold">>]}] =
            [R || #{<<"module">> := <<"mod_ok">>} = R <- Imported],
        [#{<<"observes">> := []}] = [R || #{<<"module">> := <<"m_plain">>} = R <- Imported],
        [#{<<"config">> := [First, Second, Third, Fourth]}] =
            [R || #{<<"module">> := <<"mod_ok">>} = R <- Imported],
        #{<<"module">> := <<"mod_ok">>, <<"key">> := <<"enabled">>,
          <<"type">> := <<"boolean">>, <<"default">> := <<"false">>,
          <<"has_default">> := true, <<"description">> := <<"Enable <example>.">>} = First,
        #{<<"module">> := <<"site">>, <<"default">> := <<"<<>>">>} = Second,
        #{<<"key">> := <<"legacy">>, <<"default">> := <<"#{mode => [one,two]}">>} = Third,
        #{<<"has_default">> := false} = Fourth,
        [#{<<"config">> := []}] = [R || #{<<"module">> := <<"m_plain">>} = R <- Imported],
        7 = length([R || #{<<"status">> := <<"skipped">>, <<"error">> := _} = R <- Rows]),
        io:format("Parser: all 12 source fixtures passed; symlink ignored.~n")
    after
        file:del_dir_r(Root), file:delete(Output)
    end.
write(Root, Name, Data) -> file:write_file(filename:join(Root, Name), Data).

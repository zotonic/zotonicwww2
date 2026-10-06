#!/usr/bin/env escript
%%! +S 1:1 +A 1 -noshell
%% Run from the site directory. Repository atoms remain in the parser VM.
-mode(compile).
main(_) ->
    Root = filename:join("/tmp", "external-dispatch-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Root),
    Output = Root ++ ".etf",
    try
        write(Root, "priv/dispatch/dispatch", <<"[{external_test_home, [], controller_external_test, []},\n{external_test_page, [\"page\", id, '*'], controller_external_test, [{template, \"<script>bad</script>\"}]},\n{external_test_match, [{id, \"[0-9]+\"}], controller_external_test, []}].\n">>),
        write(Root, "priv/dispatch/dispatch-api", <<"[{one, [\"one\"], controller_one, []}].\n[{two, [\"two\"], controller_two, []}].\n">>),
        write(Root, "priv/dispatch/empty", <<"[].\n">>),
        write(Root, "priv/dispatch/broken", <<"erlang:halt(99).\n">>),
        write(Root, "priv/dispatch/bad-shape", <<"[{bad, [], controller_bad}].\n">>),
        write(Root, "apps/a/priv/dispatch/dispatch", <<"[{a, [\"a\"], controller_a, []}].\n">>),
        write(Root, "apps/b/priv/dispatch/dispatch", <<"[{b, [\"b\"], controller_b, []}].\n">>),
        write(Root, "priv/dispatch/.hidden", <<"not a dispatch file">>),
        write(Root, "priv/dispatch/dispatch~", <<"not a dispatch file">>),
        ok = file:make_symlink("/etc/passwd", filename:join(Root, "priv/dispatch/symlink")),
        Port = open_port({spawn_executable, os:find_executable("escript")},
            [exit_status, {args, ["priv/bin/parse_external_docs.escript", Root, Output]}]),
        receive {Port, {exit_status, 0}} -> ok after 15000 -> error(parser_failed) end,
        {ok, Bin} = file:read_file(Output),
        Rows = binary_to_term(Bin, [safe]),
        7 = length(Rows),
        5 = length([R || #{<<"status">> := <<"parsed">>} = R <- Rows]),
        2 = length([R || #{<<"status">> := <<"skipped">>, <<"error">> := _} = R <- Rows]),
        [#{<<"rules">> := [Home, Page, Match]}] = [R || #{<<"path">> := <<"./priv/dispatch/dispatch">>} = R <- Rows],
        #{<<"path">> := <<"/">>, <<"name">> := <<"external_test_home">>} = Home,
        #{<<"path">> := <<"/page/:id/*">>, <<"controller">> := <<"controller_external_test">>} = Page,
        #{<<"path">> := <<"/:id=\"[0-9]+\"">>} = Match,
        [#{<<"rules">> := [#{<<"name">> := <<"one">>}, #{<<"name">> := <<"two">>}]}] =
            [R || #{<<"path">> := <<"./priv/dispatch/dispatch-api">>} = R <- Rows],
        io:format("Dispatch parser: 7 fixtures passed; ignored hidden, backup, and symlink files.~n")
    after
        file:del_dir_r(Root), file:delete(Output)
    end.
write(Root, Name, Data) ->
    Path = filename:join(Root, Name),
    ok = filelib:ensure_dir(Path),
    file:write_file(Path, Data).

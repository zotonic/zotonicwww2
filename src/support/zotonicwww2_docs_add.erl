%% @doc One-time module to add `-moduledoc` and `-doc` attributes to Erlang source files.
%% This file will be removed when the moduledoc changes are merged into Zotonic.
%% @end

-module(zotonicwww2_docs_add).
-export([add_moduledoc_block/2, add_callback_doc_block/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Adds a `-moduledoc` attribute after the `-module` attribute in an Erlang file.
-spec add_moduledoc_block(file:filename_all(), binary()) -> ok | {error, term()}.
add_moduledoc_block(FilePath, DocStringBin) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Split the content into lines (as binaries)
            Lines = binary:split(Content, <<"\n">>, [global]),
            %% Find the `-module` line and insert the `-doc` attribute after it
            EscapedDocString = as_binary_term(DocStringBin),
            Lines1 = insert_doc_after_module(Lines, EscapedDocString),
            %% Join the updated lines back into a single binary
            UpdatedContent = lists:join($\n, Lines1),
            %% Write the updated content back to the file
            file:write_file(FilePath, UpdatedContent);
        {error, Reason} ->
            {error, Reason}
    end.

%% Adds a `-doc` attribute before the callback in an Erlang file.
-spec add_callback_doc_block(file:filename_all(), binary(), binary()) -> ok | {error, term()}.
add_callback_doc_block(FilePath, Callback, DocStringBin) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Split the content into lines (as binaries)
            Lines = binary:split(Content, <<"\n">>, [global]),
            %% Find the `-module` line and insert the `-doc` attribute after it
            EscapedDocString = as_binary_term(DocStringBin),
            Prefix = <<"-callback ", Callback/binary, "(">>,
            Lines1 = insert_doc_before(Lines, Prefix, EscapedDocString),
            %% Join the updated lines back into a single binary
            UpdatedContent = lists:join($\n, Lines1),
            %% Write the updated content back to the file
            file:write_file(FilePath, UpdatedContent);
        {error, Reason} ->
            {error, Reason}
    end.

%% Insert the `-moduledoc` attribute after the `-module` line.
insert_doc_after_module([<<"-module(", _/binary>> = Line | Rest], DocString) ->
    [Line, [ <<"-moduledoc(\"\n">>, DocString, <<"\n\").">> ] | Rest];
insert_doc_after_module([Line | Rest], DocString) ->
    [Line | insert_doc_after_module(Rest, DocString)].

%% Insert the `-doc` attribute before a line with the given prefix.
insert_doc_before([Line | Rest], Prefix, DocString) ->
    case binary:match(Line, Prefix) of
        {0, _} ->
            [ [ <<"-doc(\"\n">>, DocString, <<"\n\").">> ], Line | Rest];
        nomatch ->
            [Line | insert_doc_before(Rest, Prefix, DocString)]
    end;
insert_doc_before([], Prefix, _DocString) ->
    ?DEBUG(#{
        text => <<"Did not find callback prefix in file.">>,
        prefix => Prefix
    }),
    [].

%% Ensure that the doc string is a valid source-binary. Keep newlines for readability.
as_binary_term(Doc) ->
    Doc1 = iolist_to_binary(io_lib:format("~s", [ z_string:trim(Doc) ])),
    Doc2 = binary:replace(Doc1, <<"\\">>, <<"\\\\">>, [ global ]),
    Doc3 = binary:replace(Doc2, <<"\"">>, <<"\\\"">>, [ global ]),
    wrap_lines(z_string:trim(Doc3)).

wrap_lines(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    WrappedLines = lists:map(fun(Line) -> wrap_line(Line) end, Lines),
    lists:join(<<"\n">>, lists:flatten(WrappedLines)).

wrap_line(<<C/utf8, _/binary>> = Line) when
    (C >= $A andalso C =< $Z);
    (C >= $a andalso C =< $z);
    (C >= $0 andalso C =< $9) ->
    case binary:match(Line, <<"|">>) of
        nomatch ->
            case size(Line) > 120 of
                true ->
                    Ls = lists:flatten(wrap_line(Line, <<>>, [])),
                    [ L || L <- Ls, L =/= <<>> ];
                false ->
                    [ Line ]
            end;
        _ ->
            [ Line ]
    end;
wrap_line(Line) ->
    [ Line ].

wrap_line(<<>>, LineAcc, Acc) ->
    lists:reverse([ LineAcc | Acc ]);
wrap_line(<<" ", Line/binary>>, LineAcc, Acc) ->
    wrap_line(Line, LineAcc, Acc);
wrap_line(S, LineAcc, Acc) ->
    case binary:split(S, <<" ">>) of
        [Before, After] ->
            SizeBefore = size(Before),
            SizeLineAcc = size(LineAcc),
            if
                SizeBefore >= 120 ->
                    wrap_line(After, <<>>, [ Before, LineAcc | Acc]);
                SizeBefore + SizeLineAcc >= 120 ->
                    wrap_line(After, Before, [ LineAcc | Acc]);
                LineAcc =:= <<>> ->
                    wrap_line(After, Before, Acc);
                true ->
                    wrap_line(After, <<LineAcc/binary, " ", Before/binary>>, Acc)
            end;
        _ ->
            wrap_line(<<>>, <<LineAcc/binary, " ", S/binary>>, Acc)
    end.



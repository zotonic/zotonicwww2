%% @doc Import repository-local documentation images as ordinary Zotonic media.
%% Relative image URLs are resolved against the documented source file. The
%% repository is the boundary: parent traversal outside it and symlinks fail.
-module(zotonicwww2_doc_media).
-export([prepare/2, prepare_external/3]).
-include_lib("kernel/include/file.hrl").

-spec prepare(map(), z:context()) -> {ok, map()} | {error, term()}.
prepare(#{source := <<"zotonic">>} = Entry, Context) ->
    prepare(Entry, m_zotonicwww2_git:git_dir(Context), Context);
prepare(Entry, _Context) ->
    {ok, Entry}.

%% External imports run while their disposable checkout still exists. Scope
%% media identity to the repository, including identical relative filenames.
-spec prepare_external(Entry, Root, Context) -> Result when
    Entry :: map(), Root :: file:filename_all(), Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
prepare_external(Entry, Root, Context) ->
    prepare(Entry, Root, Context).

prepare(#{body := Body, source_path := Source} = Entry, Root, Context) ->
    try
        Pattern = <<"<img\\b[^>]*\\bsrc=\"([^\"]+)\"[^>]*>">>,
        Matches = case re:run(Body, Pattern, [global, {capture, [0, 1], binary}]) of
            nomatch -> [];
            {match, Ms} -> Ms
        end,
        {Html, Hashes} = lists:foldl(fun([Tag, Url], {Acc, Hs}) ->
            Alt = case re:run(Tag, <<"\\balt=\"([^\"]*)\"">>, [{capture, [1], binary}]) of
                {match, [A]} -> z_html:unescape(A);
                nomatch -> <<>>
            end,
            case image(Entry, Root, z_html:unescape(Url), Alt, Context) of
                remote -> {Acc, Hs};
                {Id, Relative, Hash} ->
                    Options = z_json:encode(#{size => <<"large">>, alt => Alt, caption => <<"-">>}),
                    SafeOptions = binary:replace(binary:replace(Options, <<"<">>, <<"\\u003c">>, [global]),
                        <<">">>, <<"\\u003e">>, [global]),
                    Embed = <<"<!-- z-media ", (integer_to_binary(Id))/binary, " ", SafeOptions/binary, " -->">>,
                    WithFigures = binary:replace(Acc, <<"<p>", Tag/binary, "</p>">>, Embed, [global]),
                    {binary:replace(WithFigures, Tag, Embed, [global]), Hs#{Relative => Hash}}
            end
        end, {Body, #{}}, Matches),
        case map_size(Hashes) of
            0 -> {ok, Entry};
            _ -> {ok, Entry#{body => Html, media_hashes => Hashes}}
        end
    catch
        error:Reason -> {error, {documentation_image, Source, Reason}}
    end.

image(#{source_path := Source} = Entry, Root, Url, Alt, Context) ->
    case relative_path(Source, Url) of
        remote -> remote_image(Entry, Url, Alt, Context);
        {ok, Relative} ->
            File = checked_file(Root, Relative),
            import_file(Entry, Relative, File, Alt, Context)
    end.

import_file(Entry, Relative, File, Alt, Context) ->
    {ok, Data} = file:read_file(File),
    Hash = hex(Data),
    Identity = case maps:get(source, Entry, <<"zotonic">>) of
        <<"zotonic">> -> Relative;
        Source -> <<Source/binary, ":", Relative/binary>>
    end,
    {ok, Id} = import_image(Identity, File, Hash, Alt, Context),
    {Id, Relative, Hash}.

%% Only external documentation downloads remote images. Keep this allowlist
%% narrow: untrusted README URLs must not reach internal services or carry
%% site credentials, including via redirects. Other hosts remain remote embeds.
remote_image(#{source := <<"external_", _/binary>>} = Entry, Url, Alt, Context) ->
    case downloadable_image(Url) of
        false -> remote;
        true ->
            Options = [{max_length, 10485760}, {timeout, 30000},
                {fetch_options, [{autoredirect, false}, {authorization, none}]}],
            {ok, File, _Filename} = m_media:download_file(Url, Options, Context),
            try import_file(Entry, Url, File, Alt, Context)
            after file:delete(File) end
    end;
remote_image(_, _, _, _) -> remote.

downloadable_image(Url) ->
    try uri_string:parse(Url) of
        #{scheme := <<"https">>, host := Host, path := Path} = Parts ->
            lists:member(Host, [<<"user-images.githubusercontent.com">>,
                <<"raw.githubusercontent.com">>])
                andalso not maps:is_key(userinfo, Parts)
                andalso maps:get(port, Parts, 443) =:= 443
                andalso not maps:is_key(query, Parts)
                andalso not maps:is_key(fragment, Parts)
                andalso lists:member(string:lowercase(filename:extension(Path)),
                    [<<".jpg">>, <<".jpeg">>, <<".png">>, <<".gif">>, <<".webp">>]);
        _ -> false
    catch _:_ -> false end.

%% Root-relative URLs and remote URLs belong to the rendered site or service.
relative_path(_Source, <<"/", _/binary>>) -> remote;
relative_path(_Source, <<"#", _/binary>>) -> remote;
relative_path(Source, Url) ->
    case binary:match(Url, <<":">>) of
        nomatch ->
            Parts = filename:split(filename:join(filename:dirname(Source), Url)),
            Relative = filename:join(lists:reverse(normalize(Parts, []))),
            true = lists:member(string:lowercase(filename:extension(Relative)),
                [<<".jpg">>, <<".jpeg">>, <<".png">>, <<".gif">>, <<".webp">>]),
            {ok, Relative};
        _ -> remote
    end.

normalize([], Acc) -> Acc;
normalize([<<".">> | Rest], Acc) -> normalize(Rest, Acc);
normalize([<<"..">> | Rest], [_ | Acc]) -> normalize(Rest, Acc);
normalize([<<"..">> | _], []) -> error(outside_repository);
normalize([Part | Rest], Acc) -> normalize(Rest, [Part | Acc]).

checked_file(Root, Relative) ->
    lists:foldl(fun(Part, Parent) ->
        Path = filename:join(Parent, Part),
        {ok, #file_info{type = Type}} = file:read_link_info(Path),
        true = Type =:= directory orelse Type =:= regular,
        Path
    end, Root, filename:split(Relative)).

import_image(Relative, File, Hash, Alt, Context) ->
    Name = <<"doc_image_", (hex(Relative))/binary>>,
    Props = #{<<"name">> => Name, <<"title">> => Alt,
        <<"is_published">> => true, <<"doc_image_source">> => Relative,
        <<"doc_image_hash">> => Hash},
    case m_rsc:rid(Name, Context) of
        undefined -> m_media:insert_file(File, Props, Context);
        Id ->
            Relative = m_rsc:p(Id, doc_image_source, Context),
            case {m_rsc:p(Id, doc_image_hash, Context), m_media:get(Id, Context)} of
                {Hash, Medium} when is_map(Medium) -> {ok, Id};
                _ -> m_media:replace_file(File, Id, Props, Context)
            end
    end.

hex(Data) -> binary:encode_hex(crypto:hash(sha256, Data), lowercase).

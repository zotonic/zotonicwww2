-module(filter_zotonicwww2_by_version).

-export([
    zotonicwww2_by_version/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

zotonicwww2_by_version(List, Context) ->
    Vs = lists:map(
        fun(Id) ->
            {version(Id, Context), Id}
        end,
        List),
    {_, Ids1} = lists:unzip( lists:reverse(lists:sort(Vs)) ),
    Ids1.


version(Id, Context) ->
    Title = m_rsc:p_no_acl(Id, title, Context),
    Title1 = z_convert:to_binary( z_trans:lookup_fallback(Title, Context) ),
    vsn(Title1).

vsn(<<"Release ", Vsn/binary>>) ->
    vsn(Vsn);
vsn(<<"Notes", _/binary>>) ->
    [100000];
vsn(Vsn) ->
    Vs = binary:split( z_string:trim(Vsn), <<".">>, [ global ]),
    lists:map(
        fun z_convert:to_integer/1,
        Vs).

-module(m_zotonicwww2_search).

-behaviour(zotonic_model).

-export([
    m_get/3,

    exact_match/2
    ]).

m_get([ <<"exact_match">>, Term | Rest ], _Msg, Context) ->
    {ok, {exact_match(Term, Context), Rest}};
m_get([ <<"title_match">>, Term | Rest ], _Msg, Context) ->
    {ok, {title_match(Term, Context), Rest}}.


exact_match(Term, Context) when is_binary(Term) ->
    Lower = z_string:trim( z_string:to_lower(Term) ),
    Ids = z_db:q("
        select id
        from rsc
        where pivot_title = $1",
        [ Lower ],
        Context),
    [ Id || {Id} <- Ids ].

title_match(Term, Context) when is_binary(Term) ->
    Exact = exact_match(Term, Context),
    Lower = z_string:trim( z_string:to_lower(Term) ),
    {RefFrom, RefTo} = m_category:get_range_by_name(reference, Context),
    {CookFrom, CookTo} = m_category:get_range_by_name(cookbook, Context),
    StartIds = z_db:q("
        select id
        from rsc
        where pivot_title like $1 || '%'
          and (
                (pivot_category_nr >= $2 and pivot_category_nr <= $3)
             or (pivot_category_nr >= $4 and pivot_category_nr <= $5)
          )
        ",
        [ Lower, RefFrom, RefTo, CookFrom, CookTo ],
        Context),
    AllIds = z_db:q("
        select id
        from rsc
        where pivot_title like '%' || $1 || '%'
          and (
                (pivot_category_nr >= $2 and pivot_category_nr <= $3)
             or (pivot_category_nr >= $4 and pivot_category_nr <= $5)
          )
        ",
        [ Lower, RefFrom, RefTo, CookFrom, CookTo ],
        Context),
    Ids = StartIds ++ (AllIds -- StartIds),
    Ids1 = [ Id || {Id} <- Ids ],
    lists:sublist(Ids1 -- Exact, 50).

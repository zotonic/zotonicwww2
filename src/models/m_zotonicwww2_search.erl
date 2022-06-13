%% @doc Model for fetching and updating the Git clone of Zotonic.
%% The Git checkout is located in "priv/data/zotonic-git".
%%
%% This file is a model. You will see functions like 'm_get' that
%% implement the model behaviour. Models are always located in the
%% directory 'models' and their filename always start with 'm_'.
%%
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell

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

%% Note that the filename starts with m_, followed by the module name.
%% This ensures that there are no unexpected name clashes like with the
%% 'm_search' model in the Zotonic core.
-module(m_zotonicwww2_search).

-behaviour(zotonic_model).

-export([
    m_get/3
    ]).


%% Include the central definitions of Zotonic. Useful for macro
%% definitions like ?DEBUG.
-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Handle GET requests for this model. Can be called from the
%% templates (m.zotonicwww2_search), the API (/api/model/zotonicwww2_search/get/...)
%% or via MQTT (topic model/zotonicwww2_search/get).
%%
%% The first argument is the split path of the request (after 'get').
%%
%% The second argument is the MQTT messages, if any. For template calls
%% this could be 'undefined'. API calls also construct a MQTT message, as
%% the API routes calls via the MQTT tree and the zotonic_model.erl (in
%% zotonic_core/src/support).
%%
%% The m_get function consumes as much of the Path as is needed, it must
%% return its result together with the unconsumed part of the path. The
%% zotonic_model functions will do a further lookup of the Path remainder
%% in the return value.
%%
%% On an error an error tuple should be returned. For the template routines
%% this maps to 'undefined' and is ignored. The API and MQTT will return
%% a payload with the error to the caller.-spec m_get( Path :: list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"title_match">>, Term | Rest ], _Msg, Context) ->
    {ok, {title_match(Term, Context), Rest}}.


-spec title_match( binary(), z:context() ) -> list( m_rsc:resource_id() ).
title_match(Term, Context) when is_binary(Term) ->
    Lower = z_string:trim( z_string:to_lower(Term) ),
    {TextFrom, TextTo} = m_category:get_range_by_name(text, Context),
    {MediaFrom, MediaTo} = m_category:get_range_by_name(media, Context),
    WordIds = z_db:q("
        select id
        from rsc
        where (  pivot_title like '%' || $1 || '\\_%'
              or pivot_title like '%' || $1 || ' %'
              or pivot_title like '%\\_' || $1 || '%'
              or pivot_title like '% ' || $1 || '%'
          )
          and pivot_title <> $1
          and (
                (pivot_category_nr >= $2 and pivot_category_nr <= $3)
             or (pivot_category_nr >= $4 and pivot_category_nr <= $5)
          )
          order by length(pivot_title)
        ",
        [ Lower, TextFrom, TextTo, MediaFrom, MediaTo ],
        Context),
    AllIds = z_db:q("
        select id
        from rsc
        where pivot_title like '%' || $1 || '%'
          and pivot_title <> $1
          and (
                (pivot_category_nr >= $2 and pivot_category_nr <= $3)
             or (pivot_category_nr >= $4 and pivot_category_nr <= $5)
          )
        order by length(pivot_title)
        ",
        [ Lower, TextFrom, TextTo, MediaFrom, MediaTo ],
        Context),
    Ids = WordIds ++ (AllIds -- WordIds),
    Ids1 = [ Id || {Id} <- Ids ],
    lists:sublist(Ids1, 50).

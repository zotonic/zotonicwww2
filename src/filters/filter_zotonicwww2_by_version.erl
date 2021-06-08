%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Template filter for sorting release note resources on their version number.

%% Copyright 2021 Marc Worrell
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


% The module name of a filter MUST start with 'filter_'
% It is good habit to prefix the filter with the name of the module or site.
% In this way a name clash with other modules and sites is prevented.
-module(filter_zotonicwww2_by_version).

-export([
    % The name of the filter MUST be used as the entry point.
    zotonicwww2_by_version/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% The /2 version is for filters without extra arguments. This filter
% is used in a template like:
%
%    {{ ids|zotonicwww2_by_version }}
%
zotonicwww2_by_version(undefined, _Context) ->
    [];
zotonicwww2_by_version(List, Context) when is_list(List) ->
    Vs = lists:map(
        fun(Id) ->
            {version(Id, Context), Id}
        end,
        List),
    % Highest version number first, so reverse after sorting
    {_, Ids1} = lists:unzip( lists:reverse(lists:sort(Vs)) ),
    Ids1.


version(Id, Context) ->
    % Request the title of the release note, without checking access control
    Title = m_rsc:p_no_acl(Id, title, Context),
    % Titles are translatable and can be:
    % - a #trans{} record
    % - a binary (utf8 encoded)
    % - undefined
    %
    % The z_trans:lookup_fallback/2 call maps a #trans{} record to the
    % binary in the correct language.
    Title1 = z_trans:lookup_fallback(Title, Context),
    vsn(Title1).

% Release note titles are always of the form: "Release 9.9.9"
% Map this to integers for easy sorting.
vsn(undefined) ->
    [];
vsn(<<"Release ", Vsn/binary>>) ->
    vsn(Vsn);
vsn(<<"Notes", _/binary>>) ->
    [100000];
vsn(Vsn) ->
    Vs = binary:split( z_string:trim(Vsn), <<".">>, [ global ]),
    lists:map(
        fun z_convert:to_integer/1,
        Vs).

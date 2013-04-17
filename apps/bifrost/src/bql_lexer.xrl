Definitions.

UUID   = [0-9a-fA-F\-]
WS     = ([\000-\s]|%.*)

Rules.
{WS}+            : skip_token.
'{UUID}+'        : {token, {uuid, TokenLine, validate_uuid(strip(TokenChars, TokenLen))}}.
user             : {token, {user_kw, TokenLine, TokenChars}}.
can              : {token, {can_kw, TokenLine, TokenChars}}.
cannot           : {token, {cannot_kw, TokenLine, TokenChars}}.
and              : {token, {and_kw, TokenLine, TokenChars}}.
or               : {token, {or_kw, TokenLine, TokenChars}}.
not              : {token, {not_kw, TokenLine, TokenChars}}.
on               : {token, {on_kw, TokenLine, TokenChars}}.
typeof           : {token, {typeof_kw, TokenLine, TokenChars}}.
\[               : {token, {lstart, TokenLine, TokenChars}}.
\]               : {token, {lend, TokenLine, TokenChars}}.
,                : {token, {comma, TokenLine, TokenChars}}.
create           : {token, {create, TokenLine, TokenChars}}.
read             : {token, {read, TokenLine, TokenChars}}.
update           : {token, {update, TokenLine, TokenChars}}.
delete           : {token, {delete, TokenLine, TokenChars}}.
grant            : {token, {grant, TokenLine, TokenChars}}.
group            : {token, {group, TokenLine, TokenChars}}.
container        : {token, {container, TokenLine, TokenChars}}.
object           : {token, {object, TokenLine, TokenChars}}.
actor            : {token, {actor, TokenLine, TokenChars}}.


Erlang code.
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2011-2012 Kevin A. Smith All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-ifdef(DEV).
-define(UUID_SIZE, 3).
-else.
-define(UUID_SIZE, 32).
-endif.
strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

validate_uuid(UUID) ->
    case length(UUID) - ?UUID_SIZE of
        0 ->
            UUID;
        %% Accept hyphens
        4 ->
            UUID;
        _ ->
            error(bad_uuid)
    end.

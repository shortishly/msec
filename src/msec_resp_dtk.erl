%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(msec_resp_dtk).

-feature(maybe_expr, enable).

-export([decode/0]).
-export([decode/1]).


decode() ->
    ?FUNCTION_NAME(fun msec_storage_sync:metadata/1).

decode(Metadata) ->
    fun
        (Encoded) ->
            (scran_sequence:combined_with(
               scran_result:into_map(
                 scran_sequence:sequence(
                   [scran_sequence:separated_pair(
                      scran_result:kv(
                        database,
                        scran_character_complete:re("[^.]+")),
                      scran_character_complete:tag("."),
                      scran_result:kv(
                        table,
                        scran_character_complete:re("[^.]+")))])),
               fun
                   (DatabaseTable) ->
                       case Metadata(DatabaseTable) of
                           {ok, Mapping} ->
                               scran_result:into_map(
                                 scran_sequence:sequence(
                                   [scran_result:kv(
                                      key,
                                      scran_sequence:preceded(
                                        scran_character_complete:tag("."),
                                        msec_resp_key:decode(Mapping)))]));

                           not_found ->
                               scran_combinator:failure()
                       end
               end))(Encoded)
    end.

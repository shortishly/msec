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


-module(msec_resp_key).

-feature(maybe_expr, enable).

-export([decode/1]).


decode(Mapping) ->
    fun
        (Encoded) ->
            (scran_combinator:map_parser(
               scran_multi:separated_list1(
                 scran_character_complete:tag("/"),
                 scran_character_complete:re("[^/]+")),
               keys(primary_key_types(Mapping))))(Encoded)
    end.


primary_key_types(#{metadata := #{simple_primary_key := Positions},
                    coltypes := ColTypes}) ->
    lists:map(
      fun
          (Position) ->
              lists:nth(Position + 1, ColTypes)
      end,
      Positions).


keys(Types) ->
    fun
        (Keys) ->
            ?FUNCTION_NAME(Types, Keys, [])
    end.


keys([Type | Types], [Key | Keys], A) ->
    maybe
        {<<>>, Decoded} ?= (msmp_text:decode(#{type => Type}))(Key),
        ?FUNCTION_NAME(Types, Keys, [Decoded | A])
    end;

keys([], [], A) ->
    {<<>>, lists:reverse(A)};

keys(_, _, _) ->
    nomatch.

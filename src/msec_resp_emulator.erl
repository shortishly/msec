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


-module(msec_resp_emulator).

-feature(maybe_expr, enable).

-export([info/1]).
-export([init/1]).
-export([lookup/1]).
-export([recv/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


init([]) ->
    {ok,
     #{protocol => #{version => 2},
       requests => gen_statem:reqids_new()}}.


recv(#{command := #{name := info},
       message := {array, [_]}}) ->
    {continue,
     {encode,
      {bulk,
       ["# Server\r\nredis_version:", msec:version(), "\r\n"]}}};

recv(#{command := #{name := ping},
       message := {array, [_]}}) ->
    {continue, {encode, {string, "pong"}}};

recv(#{command := #{name := ping},
       message := {array, [_, {bulk, _} = Greeting]}}) ->
    {continue, {encode, Greeting}};

recv(#{command := #{name := command},
       message := {array, _}}) ->
    {continue, {encode, {array, []}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"3">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 3}},
     {encode,
      {map,
       [{{bulk, <<"server">>}, {bulk, <<"msec">>}},
        {{bulk, <<"version">>}, {bulk, msec:version()}},
        {{bulk,<<"proto">>}, {integer, 3}},
        {{bulk, <<"id">>}, {integer, erlang:phash2(self())}},
        {{bulk, <<"mode">>}, {bulk, <<"standalone">>}},
        {{bulk, <<"role">>}, {bulk, <<"master">>}},
        {{bulk, <<"modules">>}, {array, []}}]}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"2">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 2}},
     {encode,
      {array,
       [{bulk, <<"server">>}, {bulk, <<"msec">>},
        {bulk, <<"version">>}, {bulk, msec:version()},
        {bulk, <<"proto">>}, {integer, 2},
        {bulk, <<"id">>}, {integer, erlang:phash2(self())},
        {bulk, <<"mode">>}, {bulk, <<"standalone">>},
        {bulk, <<"role">>}, {bulk, <<"master">>},
        {bulk, <<"modules">>}, {array, []}]}}};

recv(#{command := #{name := hello},
       message := {array, [_]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 2}},
     {encode,
      {array,
       [{bulk, <<"server">>}, {bulk, <<"msec">>},
        {bulk, <<"version">>}, {bulk, msec:version()},
        {bulk, <<"proto">>}, {integer, 2},
        {bulk, <<"id">>}, {integer, erlang:phash2(self())},
        {bulk, <<"mode">>}, {bulk, <<"standalone">>},
        {bulk, <<"role">>}, {bulk, <<"master">>},
        {bulk, <<"modules">>}, {array, []}]}}};

recv(#{command := #{name := exists},
       message := {array, [_ | Keys]}}) ->
    {continue,
     {encode,
      {integer,
       lists:foldl(
         fun
             ({bulk, Key}, A) ->
                 case lookup(Key) of
                     {ok, _} ->
                         A + 1;

                     not_found ->
                         A
                 end
         end,
         0,
         Keys)}}};

recv(#{command := #{name := hexists},
       message := {array, [_, {bulk, Key}, {bulk, Field}]}}) ->
    {continue,
     {encode,
      {integer,
       case lookup(Key) of
           {ok, #{Field := Value}} when Value /= null ->
               1;

           {ok, _} ->
               0;

           not_found ->
               0
       end}}};

recv(#{command := #{name := hget},
       message := {array, [_, {bulk, Key}, {bulk, Field}]}} = Recv) ->
    ?LOG_DEBUG(#{recv => Recv}),
    {continue,
     {encode,
      {bulk,
       case lookup(Key) of
           {ok, #{Field := Value}} when Value /= null ->
               ?LOG_DEBUG(#{field => Field, value => Value}),
               Value;

           _ ->
               null
       end}}};

recv(#{command := #{name := hgetall},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {array,
       case lookup(Key) of
           {ok, Row} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (K, V, A) ->
                         [{bulk, K}, {bulk, V} | A]
                 end,
                 [],
                 Row);

           not_found ->
               []
       end}}};

recv(#{command := #{name := hlen},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {integer,
       case lookup(Key) of
           {ok, Row} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (_, _, A) ->
                         A + 1
                 end,
                 0,
                 Row);

           not_found ->
               0
       end}}};

recv(#{command := #{name := hkeys},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {array,
       case lookup(Key) of
           {ok, Row} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (K, _, A) ->
                         [{bulk, K} | A]
                 end,
                 [],
                 Row);

           not_found ->
               []
       end}}};

recv(#{message := {array, [{bulk, Command} | _]}} = Unknown) ->
    ?LOG_DEBUG(#{unknown => Unknown}),
    {continue, {encode, {error, ["unknown command '", Command, "'"]}}}.


info({notify,
      #{action := Action,
        keys := Keys,
        name := Name,
        publication := Publication}}) ->
    {continue,
     lists:map(
       fun
           (Key) ->
               {encode,
                {array,
                 [{bulk, "message"},
                  {bulk, topic("__keyspace@0__", Publication, Name, Key)},
                  {bulk, action(Action)}]}}
       end,
       Keys)};

info(Message) ->
    ?LOG_DEBUG(#{info => Message}),
    continue.


lookup(Encoded) ->
    ?LOG_DEBUG(#{encoded => Encoded}),

    maybe
        {<<>>, #{key := Key} = DTK} ?= (msec_resp_dtk:decode())(Encoded),

        ?LOG_DEBUG(#{dtk => DTK}),

        {ok, Row} ?= msec_storage_sync:read(
                       DTK#{key := case Key of
                                       [Primary] ->
                                           Primary;
                                       Compound ->
                                           list_to_tuple(Compound)
                                   end}),

        ?LOG_DEBUG(#{row => Row}),

        {ok,
         #{metadata := #{simple_primary_key := Positions,
                         column_name := Names},
           coltypes := Types} = Meta} ?= msec_storage_sync:metadata(DTK),

        ?LOG_DEBUG(#{meta => Meta}),

        {ok,
         maps:from_list(
           lists:zipwith3(
             fun
                 (Name, Type, Value) ->
                     {Name, (msmp_text:encode(#{type => Type}))(Value)}
             end,
             Names, Types, integrate(Key, tuple_to_list(Row), Positions)))}
    else
        nomatch ->
            not_found;

        not_found ->
            not_found
    end.


integrate(Keys, Columns, Positions) ->
    ?FUNCTION_NAME(Keys, Columns, Positions, 0, []).


integrate([], [], [], _, A) ->
    lists:reverse(A);

integrate([], Columns, _, _, A) ->
    lists:reverse(A) ++ Columns;

integrate(Keys, [Column | Columns], [Position | _] = Positions, N, A) when N < Position ->
    ?FUNCTION_NAME(Keys, Columns, Positions, N + 1, [Column | A]);

integrate([Key | Keys], Columns, [_ | Positions], N, A) ->
    ?FUNCTION_NAME(Keys, Columns, Positions, N + 1, [Key | A]).


topic(Prefix, Publication, Name, Key) when is_integer(Key) ->
    ?FUNCTION_NAME(Prefix, Publication, Name, integer_to_list(Key));

topic(Prefix, Publication, Name, Key) ->
    [Prefix, ":", Publication, ".", Name, ".", Key].


action(delete) ->
    "del";

action(insert_new) ->
    "set";

action(update) ->
    "set".

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


-module(msec_storage).


-export([callback_mode/0]).
-export([delete/1]).
-export([handle_event/4]).
-export([init/1]).
-export([metadata/1]).
-export([position/1]).
-export([position_update/1]).
-export([read/1]).
-export([start_link/0]).
-export([table_map/1]).
-export([write/1]).
-import(msec_statem, [nei/1]).
-import(msec_statem, [send_request/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("leveled/include/leveled.hrl").
-record(entry, {key, value, ops = 0}).


start_link() ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], []).


position(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


position_update(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


read(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


delete(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


write(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


table_map(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


metadata(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


send_request(Arg, Action) ->
    ?FUNCTION_NAME(Arg, Action, config(Action)).


send_request(Arg, Action, Config) ->
    send_request(
      maps:without(
        keys(Config),
        maybe_label(
          Arg#{request => {request, args(Action, Arg, Config)}}))).


config(position_update) ->
    [position];

config(Action) when Action == write;
                    Action == delete ->
    [row, table_id];

config(read) ->
    [database, table, key];

config(metadata) ->
    [database, table];

config(table_map) ->
    [flags,
     table,
     metadata,
     database,
     table_id,
     field_metadata,
     coltypes,
     null_bitmap];

config(position) ->
    [].


keys(Config) ->
    lists:map(
      fun
          ({Key, _}) ->
              Key;

          (Key) ->
              Key
      end,
      Config).


args(Action, Arg, Config) ->
    lists:foldl(
      fun
          ({Parameter, Default}, A) ->
              A#{Parameter => maps:get(Parameter, Arg, Default)};

          (Parameter, A) ->
              case maps:find(Parameter, Arg) of
                  {ok, Value} ->
                      A#{Parameter => Value};

                  error ->
                      error(arg_missing, [Parameter])
              end
      end,
      #{action => Action},
      Config).


maybe_label(#{requests := _, label := _} = Arg) ->
    Arg;

maybe_label(#{requests := _} = Arg) ->
    Arg#{label => ?MODULE};

maybe_label(Arg) ->
    Arg.

callback_mode() ->
    handle_event_function.


init([]) ->
    process_flag(trap_exit, true),
    {ok,
     ready,
     #{requests => gen_server:reqids_new(),
       tables => ets:new(tables, []),
       cache => ets:new(cache, [{keypos, 2}])},
     nei(leveled)}.


handle_event(internal, leveled, _, Data) ->
    case msec_sup:get_child(hd(get('$ancestors')), leveled) of
        {_, PID, worker, _} when is_pid(PID) ->
            {keep_state, Data#{storage => PID}};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, no_storage}
    end;

handle_event({call, From},
             {request,
              #{action := position_update,
                position := Position}},
             _,
             _) ->
    {keep_state_and_data,
     nei({put,
          #{from => From,
            bucket => <<"msec">>,
            key => position,
            value => Position}})};

handle_event({call, From},
             {request, #{action := position}},
             _,
             _) ->
    {keep_state_and_data,
     nei({get,
          #{from => From,
            bucket => <<"msec">>,
            key => position}})};

handle_event({call, From},
             {request,
              #{action := read,
                key := Key} = Detail},
             _,
             #{cache := Cache}) ->
    Bucket = dt(Detail),
    case ets:lookup(Cache, {Bucket, Key}) of
        [] ->
            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => miss, bucket => Bucket}}),
              nei({get, #{from => From, bucket => Bucket, key => Key}})]};

        [#entry{value = Value}] ->
            {keep_state_and_data,
             [{reply, From, {ok, Value}},
              nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => hit, bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               msec_config:timeout(expiry),
               expired}]}
    end;

handle_event({call, From},
             {request,
              #{action := table_map,
                table_id := TableId} = Mapping},
             _,
             #{tables := Tables}) ->
    ets:insert(Tables, {TableId, Mapping}),
    {keep_state_and_data,
     nei({put,
          #{from => From,
            bucket => <<"msec/mapping">>,
            key => dt(Mapping),
            value => maps:without([database, table], Mapping)}})};

handle_event({call, From},
             {request,
              #{action := metadata} = Metadata},
             _,
             #{cache := Cache}) ->
    Bucket = <<"msec/mapping">>,
    Key = dt(Metadata),
    case ets:lookup(Cache, {Bucket, Key}) of
        [] ->
            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => miss, bucket => Bucket}}),
              nei({get, #{from => From, bucket => Bucket, key => Key}})]};

        [#entry{value = Value}] ->
            {keep_state_and_data,
             [{reply, From, {ok, Value}},
              nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => hit, bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               msec_config:timeout(expiry),
               expired}]}
    end;

handle_event({call, From},
             {request,
              #{action := write,
                row := Row,
                table_id := TableId}},
             _,
             #{tables := Tables}) ->
    case ets:lookup(Tables, TableId) of
        [] ->
            {stop, {no_such_table, TableId}};

        [{_, Mapping}] ->
            BKV = #{bucket => dt(Mapping),
                    key => key(Row, Mapping),
                    value => value(Row, Mapping)},

            {keep_state_and_data,
             [nei({cache, BKV}),
              nei({put, BKV#{from => From}})]}
    end;

handle_event(internal,
             {cache,
              #{bucket := Bucket,
                key := Key,
                value := Value}},
              _,
              #{cache := Cache}) ->
    case ets:update_element(
           Cache,
           {Bucket, Key},
           {#entry.value,  Value}) of

        true ->
            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => update,
                     bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               msec_config:timeout(expiry),
               expired}]};

        false ->
            true = ets:insert_new(
                     Cache,
                     #entry{key = {Bucket, Key}, value = Value}),

            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => insert,
                     bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               msec_config:timeout(expiry),
               expired}]}
    end;

handle_event({call, From},
             {request,
              #{action := delete,
                row := Row,
                table_id := TableId}},
             _,
             #{cache := Cache, tables := Tables}) ->
    case ets:lookup(Tables, TableId) of
        [] ->
            {stop, {no_such_table, TableId}};

        [{_, Mapping}] ->
            Bucket = dt(Mapping),
            Key = key(Row, Mapping),

            ets:delete(Cache, {Bucket, Key}),

            {keep_state_and_data,
             [nei({delete,
                   #{from => From,
                     bucket => Bucket,
                     key => Key}}),
              nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => delete,
                     bucket => Bucket}}),
              {{timeout, {Bucket, Key}}, infinity, cancelled}]}
    end;

handle_event(internal,
             {put = Action,
              #{bucket := Bucket,
                key := Key,
                value := Value} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     nei({storage,
          #{request => {put, Bucket, Key, Value, [], ?STD_TAG, infinity, false},
            label => storage_label(Action, Detail)}})};

handle_event(internal,
             {delete = Action,
              #{bucket := Bucket,
                key := Key} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     nei({storage,
          #{request => {put, Bucket, Key, delete, [], ?STD_TAG, infinity, false},
            label => storage_label(Action, Detail)}})};

handle_event(internal,
             {get = Action,
              #{from := _,
                bucket := Bucket,
                key := Key} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     nei({storage,
          #{request => {get, Bucket, Key, ?STD_TAG},
            label => storage_label(Action, Detail)}})};

handle_event(internal,
             {storage, #{request := Request, label := Label}},
             _,
             #{requests := Requests, storage := Storage} = Data) ->
    {keep_state,
     Data#{requests := gen_server:send_request(
                         Storage,
                         Request,
                         Label,
                         Requests)}};

handle_event(internal, backlog = Action, _, #{requests := Requests}) ->
    {keep_state_and_data,
     nei({telemetry,
          Action,
          #{value => gen_server:reqids_size(Requests)}})};

handle_event(
  internal,
  {response,
   #{reply := {ok, Value} = Reply,
     label := #{action := get = Action,
                bucket := Bucket,
                key := Key,
                from := From}}},
  _,
  _) ->
    {keep_state_and_data,
     [{reply, From, Reply},
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}}),
      nei({cache,
           #{bucket => Bucket,
             key => Key,
             value => Value}})]};

handle_event(internal,
             {response,
              #{reply := Reply,
                label := #{action := get = Action,
                           bucket := Bucket,
                           from := From}}},
             _,
             _) ->
    {keep_state_and_data,
     [{reply, From, Reply},
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}})]};

handle_event(internal,
             {response,
              #{reply := Reply,
                label := #{action := Action,
                           bucket := Bucket,
                           from := From}}},
             _,
             _) when Action == put;
                     Action == delete ->
    {keep_state_and_data,
     [{reply, From, ok},
      nei({telemetry,
           Action,
           #{count => 1},
           #{reply => Reply, bucket => Bucket}})]};

handle_event(internal,
             {response,
              #{reply := Reply,
                label := #{action := Action,
                           bucket := Bucket}}},
             _,
             _) when Action == put;
                     Action == delete ->
    {keep_state_and_data,
     nei({telemetry,
          Action,
          #{count => 1},
          #{reply => Reply, bucket => Bucket}})};

handle_event(info, {'EXIT', _, _}, _, _) ->
    stop;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_server:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             [nei({telemetry,
                   reqids_size,
                   #{value => gen_server:reqids_size(Updated)}}),
                  nei({response, #{label => Label, reply => Reply}})]};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end;

handle_event({timeout, {Bucket, _} = Key}, expired, _, #{cache := Cache}) ->
    ets:delete(Cache, Key),
    {keep_state_and_data,
     nei({telemetry,
          cache,
          #{count => 1},
          #{action => expired,
            bucket => Bucket}})};

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             _) when is_atom(EventName) ->
    {keep_state_and_data,
     nei({telemetry, [EventName], Measurements, Metadata})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             Data) ->
    ok = telemetry:execute([msec, storage] ++ EventName,
                           Measurements,
                           maps:merge(
                             maps:with([operator, client_flags], Data),
                             Metadata)),
    keep_state_and_data.


dt(#{database := Database, table := Table}) ->
    <<"msec/", Database/bytes, "/", Table/bytes>>.


value(Tuple, #{metadata := #{simple_primary_key := Positions}}) ->
    list_to_tuple(
      lists:filtermap(
        fun
            ({Position, Value}) ->
                case lists:member(Position, Positions) of
                    true ->
                        false;

                    false ->
                        {true, Value}
                end
        end,
        lists:zip(
          lists:seq(0, tuple_size(Tuple) - 1),
          tuple_to_list(Tuple)))).


key(Tuple, #{metadata := #{simple_primary_key := [Primary]}}) ->
    element(Primary + 1, Tuple);

key(Tuple, #{metadata := #{simple_primary_key := Composite}}) ->
    list_to_tuple(
      [element(Position + 1, Tuple) || Position <- Composite]).


storage_label(Action, Detail) ->
    (maps:with([from, bucket, key], Detail))#{action => Action}.

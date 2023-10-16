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
    _ = ets:new(?MODULE, [protected, named_table]),
    {ok,
     ready,
     #{requests => gen_server:reqids_new()},
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
             _) ->
    {keep_state_and_data,
     nei({get, #{from => From, bucket => dt(Detail), key => Key}})};

handle_event({call, From},
             {request,
              #{action := table_map,
                table_id := TableId} = Mapping},
             _,
             _) ->
    ets:insert(?MODULE, {TableId, Mapping}),
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
             _) ->
    {keep_state_and_data,
     nei({get,
          #{from => From,
            bucket => <<"msec/mapping">>,
            key => dt(Metadata)}})};

handle_event({call, From},
             {request,
              #{action := write,
                row := Row,
                table_id := TableId}},
             _,
             _) ->
    case ets:lookup(?MODULE, TableId) of
        [] ->
            {stop, {no_such_table, TableId}};

        [{_, Mapping}] ->
            {keep_state_and_data,
             nei({put,
                  #{from => From,
                    bucket => dt(Mapping),
                    key => key(Row, Mapping),
                    value => value(Row, Mapping)}})}
    end;

handle_event({call, From},
             {request,
              #{action := delete,
                row := Row,
                table_id := TableId}},
             _,
             _) ->
    case ets:lookup(?MODULE, TableId) of
        [] ->
            {stop, {no_such_table, TableId}};

        [{_, Mapping}] ->
            {keep_state_and_data,
             nei({delete,
                  #{from => From,
                    bucket => dt(Mapping),
                    key => key(Row, Mapping)}})}
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

handle_event(info, {'EXIT', _, _}, _, _) ->
    stop;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_server:check_response(Msg, Existing, true) of
        {{reply, Reply}, {get, From}, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             {reply, From, Reply}};

        {{reply, _}, {put, From}, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             {reply, From, ok}};

        {{reply, _}, {delete, From}, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             {reply, From, ok}};

        {{reply, _}, Label, Updated} when Label == put; Label == delete ->
            {keep_state, Data#{requests := Updated}};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end.


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


storage_label(Action, #{from := From}) ->
    {Action, From};

storage_label(Action, _) ->
    Action.

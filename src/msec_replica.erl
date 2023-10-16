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


-module(msec_replica).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start_link/1]).
-import(msec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


start_link(Arg) ->
    gen_statem:start_link({local, ?MODULE},
                          ?MODULE,
                          [Arg],
                          envy_gen:options(?MODULE)).


callback_mode() ->
    handle_event_function.


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok,
     ready,
     #{mapped => #{},
       arg => Arg,
       requests => gen_statem:reqids_new()},
    [nei(storage), nei(connection), nei(operator)]}.


handle_event(internal, storage, _, Data) ->
    case msec_sup:get_child(msec_storage_sup, storage) of
        {_, PID, worker, _} when is_pid(PID) ->
            {keep_state, Data#{storage => PID}, nei(position)};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, no_storage}
    end;

handle_event(internal, connection, _, Data) ->
    {keep_state,
     Data#{mm => msec_sup:get_child_pid(hd(get('$ancestors')), mm)}};

handle_event(internal,
             operator = Label,
             _,
             #{requests := Requests, mm := MM} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:operator(
                         #{requests => Requests,
                           label => Label,
                           server_ref => MM})}};

handle_event(internal,
             {response, #{label := operator, reply := Operator}},
             _,
             #{position := _} = Data) ->
    {keep_state,
     Data#{operator => Operator},
     [{push_callback_module,
       msec_util:snake_case([?MODULE, Operator])},
      nei(bootstrap)]};

handle_event(internal,
             {response, #{label := operator, reply := Operator}},
             _,
             Data) ->
    {keep_state, Data#{operator => Operator}};

handle_event(
  {call, From},
  {table_map, #{table_id := TableId}},
  _,
  #{mapped := Mapped}) when is_map_key(TableId, Mapped) ->
    {keep_state_and_data, {reply, From, ok}};

handle_event(
  {call, From},
  {table_map = Action,
   #{table_id := TableId} = Mapping},
  _,
  #{mapped := Mapped} = Data) ->
    {keep_state,
     Data#{mapped := Mapped#{TableId => Mapping}},
     [nei({storage_request, Mapping#{action => Action}}),
      {reply, From, ok}]};

handle_event(
  internal,
  {storage_request, #{action := Action} = Arg},
  _,
  #{storage := Storage, requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := msec_storage:Action(
                         maps:merge(
                           #{server_ref => Storage,
                             label => Action,
                             requests => Requests},
                           maps:without(
                             [action], Arg)))}};

handle_event(
  {call, From},
  {Event, #{rows := Rows, table_id := TableId}},
  _,
  _) when (Event == write_rows orelse
           Event == write_rows_compressed_v1 orelse
           Event == write_rows_v1) ->
    {keep_state_and_data,
     [{reply, From, ok} |
      lists:map(
        fun
            (Row) ->
                nei({storage_request,
                     #{action => write,
                       row => Row,
                       table_id => TableId}})
        end,
        Rows)]};

handle_event(
  {call, From},
  {Event, #{rows := Rows, table_id := TableId}},
  _,
  _) when Event == update_rows;
          Event == update_rows_compressed_v1;
          Event == update_rows_v1 ->
    {keep_state_and_data,
     [{reply, From, ok} |
      lists:map(
        fun
            ({_, Row}) ->
                nei({storage_request,
                     #{action => write,
                       row => Row,
                       table_id => TableId}})
        end,
        Rows)]};

handle_event(
  {call, From},
  {Event, #{rows := Rows, table_id := TableId}},
  _,
  _) when Event == delete_rows;
          Event == delete_rows_compressed_v1;
          Event == delete_rows_v1 ->
    {keep_state_and_data,
     [{reply, From, ok} |
      lists:map(
        fun
            ({_, Row}) ->
                nei({storage_request,
                     #{action => delete,
                       row => Row,
                       table_id => TableId}})
        end,
        Rows)]};

handle_event(
  {call, From},
  {gtid_log, #{sid := SID, gno := GNO}},
  _,
  #{position := Existing, operator := mysql} = Data) ->
    Updated = case Existing of
                  #{SID := [Interval]} = Existing ->
                      Existing#{SID := [Interval#{finish := GNO + 1}]};

                  #{} ->
                      Existing#{SID => [#{start => GNO, finish => GNO + 1}]}
              end,
    {keep_state,
     Data#{position := Updated},
     [{reply, From, ok},
      nei({storage_request,
           #{action => position_update,
             position => Updated}})]};

handle_event(
  {call, From},
  {gtid, Position},
  _,
  #{operator := mariadb} = Data) ->
    {keep_state,
     Data#{position := Position},
     [{reply, From, ok},
      nei({storage_request,
           #{action => position_update,
             position => maps:with(
                           [domain,
                            sequence,
                            server_id],
                           Position)}})]};

handle_event(internal, position, _, _) ->
    {keep_state_and_data,
     nei({storage_request, #{action => position}})};

handle_event(internal,
             {response, #{label := position, reply := Reply}},
             _,
             #{operator := Operator} = Data) ->
    {keep_state,
     Data#{position => case Reply of
                           {ok, Position} ->
                               Position;

                           not_found ->
                               #{}
                       end},
     [{push_callback_module,
       msec_util:snake_case([?MODULE, Operator])},
      nei(bootstrap)]};

handle_event(internal,
             {response, #{label := position, reply := Reply}},
             _,
             Data) ->
    {keep_state,
     Data#{position => case Reply of
                           {ok, Position} ->
                               Position;

                           not_found ->
                               #{}
                       end}};

handle_event(internal, {response, #{label := Label}}, _, _)
  when Label == write;
       Label == delete;
       Label == table_map;
       Label == position_update ->
    keep_state_and_data;

handle_event(EventType, EventContent, State, Data) ->
    msec_replica_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).

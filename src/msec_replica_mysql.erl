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


-module(msec_replica_mysql).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event(internal, bootstrap, _, _) ->
    {keep_state_and_data,
     [nei(heartbeat),
      nei(server_id),
      nei(checksum),
      nei(gtid_mode)]};

handle_event(internal, heartbeat = Action, _, _) ->
    {keep_state_and_data,
     nei({query, Action, <<"set @master_heartbeat_period = 30000001024">>})};

handle_event(internal, server_id = Action, _, _) ->
    {keep_state_and_data,
     nei({query, Action, <<"select @@global.server_id">>})};
handle_event(internal, checksum = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"SET @master_binlog_checksum = @@global.binlog_checksum, "
            "@source_binlog_checksum = @@global.binlog_checksum">>})};

handle_event(internal, gtid_mode = Action, _, _) ->
    {keep_state_and_data,
     nei({query, Action, <<"select @@global.gtid_mode">>})};

handle_event(internal,
             {response, #{reply := {error, Reason}}},
             _,
             _) ->
    {stop, Reason};

handle_event(internal,
             {response,
              #{label := gtid_mode,
                reply := {_, [[<<"ON">>]]}}},
             _,
             _) ->
    {keep_state_and_data,
     [nei(server_uuid),
      nei(replica_uuid),
      nei(register_replica),
      nei(binlog_dump_gtid)]};

handle_event(internal, server_uuid = Action, _, _) ->
    {keep_state_and_data,
     nei({query, Action, <<"select @@global.server_uuid">>})};

handle_event(internal, replica_uuid = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
         Action,
          <<"set @replica_uuid = '",
            (msec_config:replica(uuid))/bytes,
            "'">>})};

handle_event(internal,
             binlog_dump_gtid,
             _,
             #{position := Position,
               requests := Requests,
               mm := MM} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:binlog_dump_gtid(
                         #{requests => Requests,
                           label => binlog_dump_gtid,
                           call_back => self(),
                           gtids => maps:fold(
                                      fun
                                          (SID, Intervals, A) ->
                                              [#{sid => SID,
                                                 intervals => Intervals} | A]
                                      end,
                                      [],
                                      Position),
                           server_ref => MM})}};

handle_event(internal,
             {response,
              #{label := Label,
                reply := {_, [[Row]]}}},
             _,
             Data) when Label == server_id;
                        Label == server_uuid ->
    {keep_state, Data#{Label => Row}};

handle_event(internal,
             {response, #{label := binlog_dump_gtid, reply := ok}},
             _,
             _) ->
    {keep_state_and_data, pop_callback_module};

handle_event(internal, {response, #{reply := {ok, _}}}, _, _) ->
    keep_state_and_data;

handle_event(EventType, EventContent, State, Data) ->
    msec_replica_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).

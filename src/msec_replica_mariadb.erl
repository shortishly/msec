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


-module(msec_replica_mariadb).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.

handle_event(internal, bootstrap, _, _) ->
    {keep_state_and_data,
     [nei(heartbeat),
      nei(checksum),
      nei(capability),
      nei(domain_id),
      nei(connect_state),
      nei(strict_mode),
      nei(ignore_duplicates)]};

handle_event(internal, heartbeat = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @master_heartbeat_period = 30000001024">>})};

handle_event(internal, checksum = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @master_binlog_checksum= @@global.binlog_checksum">>})};

handle_event(internal, capability = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @mariadb_slave_capability=4">>})};

handle_event(internal, domain_id = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"select @@global.gtid_domain_id">>})};

handle_event(internal,
             connect_state = Action,
             _,
             #{position := #{server_id := ServerId,
                             domain := Domain,
                             sequence := Sequence}}) ->
    {keep_state_and_data,
     nei({query,
          Action,
          io_lib:format(
            "set @slave_connect_state='~b-~b-~b'",
            [Domain, ServerId, Sequence])})};

handle_event(internal, connect_state = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @slave_connect_state=''">>})};

handle_event(internal, strict_mode = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @slave_gtid_strict_mode=0">>})};

handle_event(internal, ignore_duplicates = Action, _, _) ->
    {keep_state_and_data,
     nei({query,
          Action,
          <<"set @slave_gtid_ignore_duplicates=0">>})};

handle_event(internal,
             {response, #{reply := {error, Reason}}},
             _,
             _) ->
    {stop, Reason};


handle_event(internal,
             {response,
              #{label := domain_id = Action,
                reply := {_, [[DomainId]]}}},
             _,
             Data) ->
    {keep_state, Data#{Action => DomainId}};

handle_event(internal,
             {response, #{label := ignore_duplicates, reply := {ok, _}}},
             _,
             _) ->
    {keep_state_and_data, [nei(register_replica), nei(binlog_dump)]};

handle_event(internal, {response, #{reply := {ok, _}}}, _, _) ->
    keep_state_and_data;

handle_event(internal,
             binlog_dump,
             _,
             #{requests := Requests, mm := MM} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:binlog_dump(
                         #{requests => Requests,
                           label => binlog_dump,
                           call_back => self(),
                           server_ref => MM})}};

handle_event(internal,
             {response, #{label := binlog_dump, reply := ok}},
             _,
             _) ->
    {keep_state_and_data, pop_callback_module};


handle_event(internal,
             {query, Label, SQL},
             _,
             #{requests := Requests, mm := MM} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:query(
                         #{requests => Requests,
                           label => Label,
                           server_ref => MM,
                           query => SQL})}};

handle_event(EventType, EventContent, State, Data) ->
    msec_replica_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).

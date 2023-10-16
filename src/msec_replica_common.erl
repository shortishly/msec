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


-module(msec_replica_common).


-export([handle_event/4]).
-import(msec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


handle_event({call, From}, {Action, _}, _, _)
  when Action == query;
       Action == xid;
       Action == gtid_list;
       Action == binlog_checkpoint;
       Action == rotate;
       Action == format_description;
       Action == heartbeat_log;
       Action == heartbeat ->
    {keep_state_and_data, {reply, From, ok}};

handle_event({call, From}, {Action, #{} = Detail}, _State, _Data) ->
    ?LOG_WARNING(#{action => Action, detail => Detail}),
    {keep_state_and_data, {reply, From, ok}};

handle_event(internal,
             register_replica = Label,
             _,
             #{requests := Requests, mm := MM} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:register_replica(
                         #{requests => Requests,
                           label => Label,
                           server_ref => MM})}};

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

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             nei({response, #{label => Label, reply => Reply}})};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end.

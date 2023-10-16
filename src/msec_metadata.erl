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


-module(msec_metadata).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([lookup/1]).
-export([start_link/0]).


lookup(#{database := Database, table := Table}) ->
    ets:lookup(?MODULE, {Database, Table});

lookup(TableId) when is_integer(TableId) ->
    ets:lookup(?MODULE, TableId).


start_link() ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], []).


callback_mode() ->
    handle_event_function.


init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(?MODULE, [protected, named_table]),
    {ok,
     ready,
     #{requests => gen_statem:reqids_new()}}.


handle_event({call, From},
             {table_map,
              #{table := Table,
                database := Database,
                table_id := TableId} = Mapping},
             _,
             _) ->
    ets:insert(
      ?MODULE,
      [{TableId, maps:without([table_id], Mapping)},
       {{Database, Table}, maps:without([database, table], Mapping)}]),
    {keep_state_and_data, {reply, From, ok}}.

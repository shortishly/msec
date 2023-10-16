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


-module(msec_config).


-export([database/1]).
-export([enabled/1]).
-export([leveled/1]).
-export([replica/1]).
-import(envy, [envy/1]).


database(uri = Name) ->
    envy(#{caller => ?MODULE,
           type => binary,
           names => [?FUNCTION_NAME, Name]}).


replica(uuid = Name) ->
    envy(#{caller => ?MODULE,
           type => binary,
           names => [?FUNCTION_NAME, Name]}).


leveled(root_path = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => "/data"});

leveled(log_level = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => warn}).


enabled(Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => true}).

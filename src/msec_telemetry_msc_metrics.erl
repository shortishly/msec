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


-module(msec_telemetry_msc_metrics).


-export([handle/4]).
-include_lib("kernel/include/logger.hrl").


handle([msc, socket, Type] = EventName,
       #{bytes := Bytes} = Measurements,
       #{telemetry := #{net := #{peer := #{port := Port}}}} = Metadata,
       Config) when Type == recv;
                    Type == send ->
    %% Cumulative bytes received
    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),

    metrics:counter(
      [#{name => msec_util:snake_case(EventName ++ [bytes]),
         label => #{port => Port},
         delta => Bytes}]);

handle([msc, socket, Type] = EventName,
       #{count := N} = Measurements,
       Metadata,
       Config) when Type == error;
                    Type == connect ->
    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),
    metrics:counter(
      [#{name => msec_util:snake_case(EventName ++ [count]),
         label => maps:with([reason], Metadata),
         delta => N}]);

handle([msc, mm, Type] = EventName,
       #{count := N} = Measurements,
       Metadata,
       Config) when Type == recv;
                    Type == send;
                    Type == upgrade ->
    %% Maintain a count of each type of message received.
    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),

    metrics:counter(
      [#{name => msec_util:snake_case(EventName ++ [count]),
         label => maps:merge(
                    maps:with([action], Measurements),
                    maps:with([operator], Metadata)),
         delta => N}]);

%% Fall through clause to log any missed telemetry events from msc.
%%
handle(EventName, Measurements, Metadata, Config) ->
    ?LOG_INFO(#{event_name => EventName,
                measurements => Measurements,
                metadata => Metadata,
                config => Config}).

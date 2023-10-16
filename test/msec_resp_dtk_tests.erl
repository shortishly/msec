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


-module(msec_resp_dtk_tests).


-include_lib("eunit/include/eunit.hrl").


cities_decode_test() ->
    Mapping = fun
                  (#{database := <<"shortishly">> = Database,
                     table := <<"cities">> = Table}) ->
                      {ok,
                       #{flags => 1,
                         table => Table,
                         metadata => #{simple_primary_key => "\b\t",
                                       unsignedness => #{1 => false,
                                                         2 => false,
                                                         3 => false,
                                                         5 => false,
                                                         6 => false,
                                                         7 => false},
                                       column_name => [<<"lat_d">>,
                                                       <<"lat_m">>,
                                                       <<"lat_s">>,
                                                       <<"ns">>,
                                                       <<"lon_d">>,
                                                       <<"lon_m">>,
                                                       <<"lon_s">>,
                                                       <<"ew">>,
                                                       <<"city">>,
                                                       <<"state">>],
                                       default_charset => "ÿ",
                                       column_visibility => <<"ÿÀ">>},
                         table_id => 115,
                         database => Database,
                         field_metadata => #{1 => undefined,
                                             2 => undefined,
                                             3 => undefined,
                                             4 => 2,
                                             5 => undefined,
                                             6 => undefined,
                                             7 => undefined,
                                             8 => 2,
                                             9 => 200,
                                             10 => 8},
                         coltypes => [long,
                                      long,
                                      long,
                                      blob,
                                      long,
                                      long,
                                      long,
                                      blob,
                                      varchar,
                                      varchar],
                         null_bitmap => <<255,0>>}}
              end,
    ?assertEqual(
       {<<>>,
        #{table => <<"cities">>,
          key => [<<"Tulsa">>, <<"OK">>],
          database => <<"shortishly">>}},
       (msec_resp_dtk:decode(Mapping))
         (<<"shortishly.cities.Tulsa/OK">>)).


hooke_decode_test() ->
    Metadata = fun
                   (#{database := <<"shortishly">> = Database,
                      table := <<"hooke">> = Table}) ->
                       {ok,
                        #{flags => 1,
                          table => Table,
                          metadata => #{simple_primary_key => [0],
                             unsignedness => #{1 => false,
                                               2 => false,
                                               3 => false,
                                               4 => false},
                             column_name => [<<"i">>,
                                             <<"mass">>,
                                             <<"spring1">>,
                                             <<"spring2">>],
                                        column_visibility => <<"ð">>},
                          table_id => 120,
                          database => Database,
                          field_metadata => #{1 => undefined,
                                              2 => undefined,
                                              3 => undefined,
                                              4 => undefined},
                          coltypes => [long,
                                       float,
                                       float,
                                       float],
                          null_bitmap => <<14>>}};
                   (#{}) ->
                       not_found
               end,
    ?assertEqual(
       {<<>>,
        #{table => <<"hooke">>,
          key => [1],
          database => <<"shortishly">>}},
       (msec_resp_dtk:decode(Metadata))
         (<<"shortishly.hooke.1">>)).

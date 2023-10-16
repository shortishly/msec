-- -*- mode: sql; sql-product: mysql; -*-
-- Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

use shortishly;

create table world_cup_teams_2022 (
  country varchar (50) primary key,
  top_searched text,
  second text,
  third text,
  fourth text,
  fifth text);

load data infile '/mnt/googletrends/2022-11-21 World Cup teams by country past week.csv'
  into table world_cup_teams_2022
  fields terminated by ',' optionally enclosed by '"'
  ignore 1 lines;

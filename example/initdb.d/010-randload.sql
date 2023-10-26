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

create table randload (
  id serial primary key,
  x integer,
  uuid varchar (36) default (uuid()),
  created_time timestamp default current_timestamp,
  updated_time timestamp default current_timestamp
);

set @@cte_max_recursion_depth = 1000000;

insert into randload (x)
with recursive seq as (
  select 1 as x
           union all
  select 1+x from seq limit 1000000
)
select * from seq;

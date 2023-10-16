#!/usr/bin/env bats
# -*- mode: shell-script; sh-shell: bash; -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


@test "hget shortishly.hooke.1 mass" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "0.0" ]
}

@test "hget shortishly.hooke.1 spring1" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "0.05" ]
}

@test "hget shortishly.hooke.1 spring2" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "0.05" ]
}


@test "hget shortishly.hooke.10 mass" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "4.41" ]
}

@test "hget shortishly.hooke.10 spring1" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "0.238" ]
}

@test "hget shortishly.hooke.10 spring2" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "0.232" ]
}

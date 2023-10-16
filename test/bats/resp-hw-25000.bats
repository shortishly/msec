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


@test "hget shortishly.hw_25000.1 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "65.7833" ]
}

@test "hget shortishly.hw_25000.1 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "112.993" ]
}


@test "hget shortishly.hw_25000.1999 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "69.2421" ]
}

@test "hget shortishly.hw_25000.1999 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "132.684" ]
}


@test "hget shortishly.hw_25000.15342 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "67.4263" ]
}

@test "hget shortishly.hw_25000.15342 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "110.173" ]
}


@test "hget shortishly.hw_25000.24999 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "67.5292" ]
}

@test "hget shortishly.hw_25000.24999 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "132.268" ]
}

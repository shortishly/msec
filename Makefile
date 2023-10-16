#-*- mode: makefile-gmake -*-
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
#

PROJECT = msec
PROJECT_DESCRIPTION = MySQL/MariaDB Edge Cache
PROJECT_VERSION = ${shell git describe --tags}

COVER = 1
COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct
EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

DEPS += leveled
DEPS += metrics
DEPS += msc
DEPS += msmp
DEPS += narcs
DEPS += resp
DEPS += rfc4122
DEPS += scran

SHELL_DEPS += sync

LOCAL_DEPS += ssl

dep_leveled = $(if $(DEP_LN),ln ../../../martinsumner/leveled,git https://github.com/martinsumner/leveled.git)
dep_metrics = $(if $(DEP_LN),ln ../../metrics,git https://github.com/shortishly/metrics.git)
dep_msc = $(if $(DEP_LN),ln ../../msc,git https://github.com/shortishly/msc.git)
dep_msmp = $(if $(DEP_LN),ln ../../msmp,git https://github.com/shortishly/msmp.git)
dep_narcs = $(if $(DEP_LN),ln ../../narcs,git https://github.com/shortishly/narcs.git)
dep_resp = $(if $(DEP_LN),ln ../../resp,git https://github.com/shortishly/resp.git)
dep_rfc4122 = $(if $(DEP_LN),ln ../../rfc4122,git https://github.com/shortishly/rfc4122.git)
dep_scran = $(if $(DEP_LN),ln ../../scran,git https://github.com/shortishly/scran.git)

PLT_APPS += any
PLT_APPS += asn1
PLT_APPS += backoff
PLT_APPS += bbmustache
PLT_APPS += compiler
PLT_APPS += cowlib
PLT_APPS += crypto
PLT_APPS += inets
PLT_APPS += leveled
PLT_APPS += mnesia
PLT_APPS += msc
PLT_APPS += msmp
PLT_APPS += phrase
PLT_APPS += public_key
PLT_APPS += ranch
PLT_APPS += runtime_tools
PLT_APPS += sasl
PLT_APPS += scran
PLT_APPS += ssl
PLT_APPS += stdlib
PLT_APPS += syntax_tools
PLT_APPS += tools

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -enable-feature maybe_expr
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync


BUILD_DEPS += relx
RELX_TAR = 0


include erlang.mk

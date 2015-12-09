PROJECT = esmpp_lib

DEPS = lager
dep_lager = https://github.com/basho/lager.git 2.2.0

ERLC_OPTS = "+{parse_transform, lager_transform}" "+debug_info"

include erlang.mk

PROJECT = influx

SHELL_OPTS = -s ${PROJECT}

DEPS = hackney jsx

dep_hackney = git http://github.com/strange/hackney.git

include erlang.mk

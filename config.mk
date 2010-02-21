## Template configuration

## Erlang Configuration
ERLC ?= erlc
EFLAGS ?= -Wall

EDEPS=

## Revision Macro TAG
#REV ?= #$(shell hg identify -n -i|awk '{print "\\\""$$2":"$$1"\\\""};')
REV ?= "nil"

## Standard system Configuration
SED ?= sed
RM ?= rm

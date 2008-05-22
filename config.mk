## Template configuration

## Erlang Configuration
ERLC ?= erlc
EFLAGS ?= -Wall

## Revision Macro TAG
REV=$(shell hg identify -n -i|awk '{print "\\\""$$2":"$$1"\\\""};')

## Standard system Configuration
SED ?= sed
RM ?= rm

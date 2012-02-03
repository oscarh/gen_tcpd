REBAR_URL ?= http://cloud.github.com/downloads/basho/rebar/rebar

ifneq ($(shell which wget 2>/dev/null),)
REBAR_GET ?= wget -q $(REBAR_URL)
else
REBAR_GET ?= curl -s -f $(REBAR_URL) >rebar
endif

.PHONY: all compile doc test clean clean-all

all: compile doc

rebar:
	$(REBAR_GET)
	chmod +x rebar

compile: rebar
	./rebar compile

doc: rebar
	./rebar doc

test: rebar
	./rebar eunit

clean: rebar
	./rebar clean

clean-all:
	rm -rf rebar ebin doc/*{-info,.html,.css,.png}

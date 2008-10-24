##
## Template Makefile for MyCo build system
## Author: Martin Carlson <martin@martinc.eu>
##

# Versioning and Configuration
# (use ?= when defining variables to retain override effect)
sinclude $(TOP_DIR)/config.local.mk # (optional)
sinclude config.local.mk # (optional)
include config.mk
include vsn.mk

## Code layout
APPSRC = $(patsubst src/%.app.src,%.app.src,$(wildcard src/*.app.src))
ERLS = $(patsubst src/%.erl,%.erl,$(wildcard src/*.erl))
BEAMS = $(ERLS:.erl=.beam)
MODS = $(BEAMS:.beam=)
APP = $(APPSRC:.app.src=.app)

## Dependecy Search Paths
VPATH = src:include:ebin


.PHONY: all clean c_src doc
.SUFFIXES: .erl .beam .app.src .app

all: depend $(BEAMS) $(APP) c_src

doc: 
	@echo [EDOC] gen_tcpd
	@erl -noinput -eval 'edoc:application(gen_tcpd, "./", [{doc, "doc/"}])' -s erlang halt

clean: 
	@for i in $(wildcard ebin/*); do \
		echo [RM] $$i; \
		$(RM) $$i; \
	done
	@echo [RM] depend
	@$(RM) depend
	@if test -d c_src ; then \
		$(MAKE) -C c_src clean; \
	fi
	@echo [RM] doc/
	@$(RM) doc/*.html doc/*.css doc/*.jpg doc/edoc-info doc/*.png

%.beam: %.erl
	@echo [ERLC] $<
	@$(ERLC) -o ebin $(EFLAGS) \
		-I include \
		-DREV=$(REV) \
		$(patsubst %,-pa $(TOP_DIR)/%/ebin, $(EDEPS)) $<

$(APP): $(APPSRC) vsn.mk
	@echo [SED] $<
	@$(SED) "s|%MODULES%|`echo $(MODS) | tr '[:blank:]' ','`|g" $< | \
	$(SED) "s|%VSN%|$(VSN)|g" > ebin/$@

sinclude depend
depend: $(ERLS)
	@(sh erldep src)

c_src:
	@if test -d c_src ; then \
		$(MAKE) -C c_src; \
	fi


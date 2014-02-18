REBAR=$(shell which rebar)
ifeq ($(REBAR),)
REBAR=./rebar
endif

.PHONY: all compile doc clean distclean test get-deps get-test-deps

all: compile doc test

rebuild: distclean get-test-deps compile test

get-test-deps:
	$(REBAR) -C rebar.test.config get-deps compile

get-deps:
	$(REBAR) get-deps compile

compile: get-deps
	@$(REBAR) skip_deps=true compile

doc: compile
	@$(REBAR) skip_deps=true doc

test: get-test-deps
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) skip_deps=true clean

distclean: clean
	@rm -rf $(CURDIR)/deps

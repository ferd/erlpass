REBAR=$(shell which rebar)
ifeq ($(REBAR),)
REBAR=./rebar
endif

.PHONY: all compile doc clean distclean test get-deps get-test-deps plt dialyze

PLT_APPS?=erts kernel stdlib crypto compiler deps/bcrypt
PLT_OPTS?=--verbose --statistics

DIALYZER_WARNINGS?=-Werror_handling -Wrace_conditions -Wunmatched_returns
DIALYZER_OPTS?=--verbose --statistics --no_native
DIALYZER_DIRS?=ebin/

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

plt:
	dialyzer $(PLT_OPTS) --build_plt --apps $(PLT_APPS)

dialyze: compile
	dialyzer $(DIALYZER_OPTS) $(DIALYZER_DIRS) $(DIALYZER_WARNINGS)

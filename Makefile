ERLOPTS:= -config app.config -boot start_sasl

rebar = rebar

default: compile

get-deps:
	$(rebar) get-deps

compile:
	$(rebar) compile

clean:
	$(rebar) clean

nodeps:
	$(rebar) compile skip_deps=true

test:
	EUNIT_FLAGS="$(ERLOPTS)" $(rebar) skip_deps=true eunit

test_%:
	$(rebar) skip_deps=true suite=$* eunit

perf:
	ERL_FLAGS="$(ERLOPTS)" $(rebar) skip_deps=true perf

perf_%:
	ERL_FLAGS="$(ERLOPTS)" $(rebar) skip_deps=true suite=$* perf
.PHONY: test

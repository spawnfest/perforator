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
	$(rebar) skip_deps=true eunit

test_%:
	$(rebar) skip_deps=true suite=$* eunit

.PHONY: test
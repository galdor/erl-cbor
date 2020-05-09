
all: dialyzer test doc

dialyzer:
	rebar3 dialyzer

build:
	rebar3 compile

test:
	rebar3 eunit

cover:
	rebar3 eunit --cover
	rebar3 cover

doc:
	rebar3 edoc

clean:
	$(RM) -r _build

.PHONY: all dialyzer build test cover doc clean

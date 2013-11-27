.PHONY: deps

all: deps
	./rebar compile

deps:
	./rebar get-deps

nodeps:
	./rebar skip_deps=true compile

shortcut:
	./bin/shortcut.sh $(riakpath)

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

test: all cleantest
	./rebar skip_deps=true eunit

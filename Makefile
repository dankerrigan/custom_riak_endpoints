.PHONY: deps

all: deps
	./rebar compile

deps:
	./rebar get-deps

nodeps:
	./rebar skip_deps=true compile

shortcut:
	./bin/shortcut.sh -r $(riakpath)

test: nodeps
	./bin/delete_objects.sh
	./bin/put_objects.sh
	./bin/get_objects.sh

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

custom_riak_endpoints
=====================

### Getting Started

Build the modified Riak

```
git clone https://github.com/drewkerrigan/riak.git
cd riak && make rel && cd ../
```

Build the custom_riak_endpoints project

```
git clone https://github.com/drewkerrigan/custom_riak_endpoints.git
cd custom_riak_endpoints && make
```

### Start Riak

```
../riak/rel/riak/bin/riak start
```

### Test it out

```
make test
```

### Exercises

* Modify `custom_riak_endpoints/src/cre_wm_multi.erl` to allow for multi object put
* Modify `custom_riak_endpoints/src/cre_wm_multi.erl` to allow for multi object delete

### Development Shortcuts

In order to rapidly build and redeploy changes to the custom_riak_endpoints project without having to recompile all dependencies, it is possible to directly inject the relevant beam files into Riak's release directory:

```
make shortcut riakpath="~/src/erlang/riak/"
```

#### Debugging through old fashioned way, print statements

Log debug statements:

	lager:info("What happend? ~p~n", [HowDoesErlangEvenWork])

Then watch the log:

	tail -f ../riak/rel/riak/log/console.log
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

### Test it out

```
make test
```

### Exercises

* Modify `custom_riak_endpoints/src/cre_wm_multi.erl` to allow for multi object put
* Modify `custom_riak_endpoints/src/cre_wm_multi.erl` to allow for multi object delete
#!/bin/bash

curl -XPUT http://localhost:8098/buckets/test/props \
    -H "Content-Type: application/json" \
    -d '{"props":{"allow_mult":false}}'

echo "Putting Objects..."

set -v

curl -XPUT http://localhost:8098/buckets/test/keys/obj1 \
    -H "Content-Type: application/json" \
    -d 'Test Object One'

curl -XPUT http://localhost:8098/buckets/test/keys/obj2 \
    -H "Content-Type: application/json" \
    -d 'Test Object Two'

curl -XPUT http://localhost:8098/buckets/test/keys/obj3 \
    -H "Content-Type: application/json" \
    -d 'Test Object Three'

set +v

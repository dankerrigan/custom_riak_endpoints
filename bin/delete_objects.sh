#!/bin/bash

echo "Deleting Objects..."

curl -XDELETE http://localhost:8098/buckets/test/keys/obj1
curl -XDELETE http://localhost:8098/buckets/test/keys/obj2
curl -XDELETE http://localhost:8098/buckets/test/keys/obj3

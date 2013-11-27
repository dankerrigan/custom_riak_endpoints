#!/bin/bash

echo "Getting Objects..."

set -v

curl -XGET http://localhost:8098/cre/test/obj1,obj2,obj3 \
  -H "Accept: application/json"

set +v

echo ""
echo "Done"

#!/bin/bash

## DESCRIPTION: Given a Riak source directory, moves beam files around for more rapid development

## AUTHOR: Drew Kerrigan

declare -r SCRIPT_NAME=$(basename "$BASH_SOURCE" .sh)

## exit the shell(default status code: 1) after printing the message to stderr
bail() {
    echo -ne "$1" >&2
    exit ${2-1}
}

## help message
declare -r HELP_MSG="
Given a Riak source directory, moves beam files around for more rapid development

Pre-Requisites: run 'make rel' on riak

Example: ./bin/$SCRIPT_NAME.sh -r ~/src/erlang/riak/

Usage: $SCRIPT_NAME [OPTION]... [ARG]...
  -r    riak source directory
"

## print the usage and exit the shell(default status code: 2)
usage() {
    declare status=2
    if [[ "$1" =~ ^[0-9]+$ ]]; then
        status=$1
        shift
    fi
    bail "${1}$HELP_MSG" $status
}

mode=partial

while getopts ":r:" opt; do
    case $opt in
        r)
            riak_path=${OPTARG}
            ;;
        *)
            usage 0
            ;;
    esac
done

#==========MAIN CODE BELOW==========

echo "Running make nodeps"
make nodeps

echo "Cleaning Riak's custom_riak_endpoints ebin"
rm ${riak_path}rel/riak/lib/custom_riak_endpoints-1/ebin/*

echo "Copying new ebin"
cp -R ebin/* ${riak_path}rel/riak/lib/custom_riak_endpoints-1/ebin/

echo "Stopping Riak"
bash ${riak_path}rel/riak/bin/riak stop

ulimit -n 4096
echo "Starting Riak"
bash ${riak_path}rel/riak/bin/riak start

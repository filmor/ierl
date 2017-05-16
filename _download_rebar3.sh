#!/bin/sh

curl -o `dirname $0`/rebar3 'https://s3.amazonaws.com/rebar3/rebar3'
chmod +x rebar3

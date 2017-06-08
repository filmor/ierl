#!/bin/bash

mv _checkouts{,.backup}
escript rebar3 hex publish
mv _checkouts{.backup,}

#!/bin/bash

mv _checkouts{,.backup}
escript rebar3 hex cut
mv _checkouts{.backup,}

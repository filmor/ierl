#!/bin/sh

escript rebar3 compile

for lang in erlang lfe elixir
do
    escript _build/default/bin/ierl install $lang --user --replace --name ${lang}_test
done

pytest python-tests

for lang in erlang lfe elixir
do
    yes | jupyter kernelspec uninstall ${lang}_test
done

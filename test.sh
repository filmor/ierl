#!/bin/sh

escript rebar3 compile

for lang in erlang lfe elixir
do
    escript _build/default/bin/ierl install $lang --name ${lang}_test
done

pytest

for lang in erlang lfe elixir
do
    yes | jupyter kernelspec uninstall ${lang}_test
done

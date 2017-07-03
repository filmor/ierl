# BEAM Jupyter Kernels Tool

[![Build Status](https://travis-ci.org/filmor/ierl.svg?branch=master)](https://travis-ci.org/filmor/ierl)

`ierl` is a command line tool based on [erlang-jupyter](https://github.com/filmor/erlang-jupyter) to allow running and installing
Jupyter kernels on the Erlang Virtual Machine.

Currently, three kernels have been implemented to the point that they support running code, compiling modules, code completion:

## Erlang

Not a full replacement for the built-in shell, yet, as records are not supported and pseudo-functions like `f` or `l` are not implemented.

## Elixir

The path to the Elixir installation is currently hard-coded to its default Windows installation path. Sorry for that. I'll work on
both better discovery and embedding a small Elixir distribution.

## LFE

LFE is completely embedded and can be run without any further installation.


# Usage

The released escript can be run directly by either making it executable (`chmod +x ierl`) and using it directly (`./ierl`) or starting
it explicitly with `escript ierl`. It will present the available commands and backends.

To install an Erlang kernel, run

    ./ierl install erlang

Analogously kernels for LFE and Elixir can be installed.

To specify the name of the installed kernelspec, pass it using `--name`. By default it will match the backend name (so `erlang`,
`elixir` or `lfe`).

    ./ierl install erlang --name my_erlang_kernel
    
Remoting is also supported. Pass the node to connect to via `--node` and the cookie to use via `--cookie`.

    ./ierl install erlang --node remote_node@REMOTEHOST --cookie my_secret_cookie

The installed kernels will be immediately avaialable in the Jupyter Notebook, to use them in the console run

    jupyter console --kernel my_erlang_kernel

# BEAM Jupyter Kernels Tool

[![Build Status](https://travis-ci.org/filmor/ierl.svg?branch=master)](https://travis-ci.org/filmor/ierl)
[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/filmor/ierl/master)

`ierl` is a command line tool based on
[erlang-jupyter](https://github.com/filmor/erlang-jupyter) to allow running and
installing Jupyter kernels on the Erlang Virtual Machine written in pure Erlang.
A precompiled version can be downloaded from [the release page](https://github.com/filmor/ierl/releases/latest),
download the `ierl` escript.

Currently, three kernels have been implemented to the point that they support
running code, compiling modules, and code completion:

### Erlang

Currently, only the `f` shell function to forget a variable is implemented.
However, in contrast to the usual `erl` shell, this backend also supports
defining modules inline, i.e. one can have a cell

    -module(my_mod).

    -export([func/1]).

    func(X) -> X.

and it will be compiled and usable after executing.

### Elixir

Since it's currently impossible to bundle Elixir, it will try to guess the path
by running the Elixir executable, so make sure that Elixir is in your `PATH`
environment variable. It is also possible to override the Elixir installation
path using the `--path` switch on installing the kernel.

The Jupyter Notebook will currently highlight the code as Ruby for lack of a
built-in Elixir mode in CodeMirror, but I will look into bundling 
[codemirror-mode-elixir](https://github.com/optick/codemirror-mode-elixir) in
the future.

### LFE

LFE is completely bundled and can be run without any further installation. The
highlighting is currently just straight Common Lisp, which means quite a few
things are not highlighted correctly (like `defmodule`, `defrecord`, atoms,
etc.).

### Common functionality

All kernels can access the `jup_display` functions to print non-text output (in
particular on the Jupyter Notebook):

```erlang
% Print 
DisplayRef = jup_display:display(#{ html => "<h1>Some Text</h1>" }).
```

The key of the passed map is either `html`, `text`, or a binary or string
indicating an actual MIME type like `text/html`. The value is an IO list with
data matching the MIME type. A displayed value can be updated within the same
cell by using the returned reference:

```erlang
jup_display:update(DisplayRef, #{ html => "<h1>Updated Text</h1>" }).
```

Currently, the Jupyter Notebook will also update output sections that were
initialised before, but it's not specified, whether this implementation detail
will stay.

## Usage

The released escript can be run directly by either making it executable
(`chmod +x ierl`) and using it directly (`./ierl`) or starting it explicitly
with `escript ierl` (needed on Windows, for example). It will present the
available commands and backends.

To install an Erlang kernel, run

```bash
./ierl install erlang
```

Analogously kernels for LFE and Elixir can be installed.

To specify the name of the installed kernelspec, pass it using `--name`. By
default it will match the backend name (so `erlang`, `elixir` or `lfe`).

```bash
./ierl install erlang --name my_erlang_kernel
```

Remoting is also supported. Pass the node to connect to via `--node` and the
cookie to use via `--cookie`.

```bash
./ierl install erlang --node remote_node@REMOTEHOST --cookie my_secret_cookie
```

If no name is given, it will be inferred from the kernel name and the node the
kernel is supposed to run against.

The installed kernels will be immediately available in the Jupyter Notebook, to
use them in the console run

```bash
jupyter console --kernel my_erlang_kernel
```

## Building

Clone this repository and run

```bash
./_download_rebar3.sh
```

to download the most current [`rebar3`](http://www.rebar3.org). After this, run

```bash
./rebar3 escriptize
```

to download the dependencies and compile the `escript`. It will be created in
`_build/default/bin/ierl`.

For development it might make sense to check out a local instance of
[`erlang-jupyter`](https://github.com/filmor/erlang-jupyter) by running

```bash
./_checkout_jupyter.sh
```

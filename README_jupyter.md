[![Build Status](https://travis-ci.com/filmor/erlang-jupyter.svg?branch=master)](https://travis-ci.com/filmor/erlang-jupyter)

# erlang-jupyter
Erlang implementation of the [Jupyter Messaging
Specification](http://jupyter-client.readthedocs.io/en/latest/messaging.html),
enabling writing Jupyter kernels in Erlang or other BEAM based languages. Common
functionality like history keeping, the execution counter and output are handled
in this library such that one can concentrate on the individual behaviour.

## Usage
To implement a kernel, the behaviour `jup_kernel_backend` has to be implemented.
The only required functions are `do_execute` and `do_kernel_info`.

Running a kernel is then a matter of running `jupyter:start_kernel` with an
arbitrary name, the filename of a Jupyter connection file, the backend module
and additional arguments that are passed to the backend's `init` callback.

Currently, the API is in flux, so there is no concrete documentation on the
individual functions, yet. It's currently developed in lockstep with
[ierl](https://github.com/filmor/ierl), so the kernel backends there (Erlang,
LFE and Elixir) are the de-facto documentation until the behaviour is done.
